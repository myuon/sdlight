{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Layer
  ( Layer
  , newLayer
  , resizeLayer
  , renderLayer

  , Layered
  , layered
  , newLayered
  , renderLayered
  , Eff'Layered
  , wfLayered

  , Delayed
  , delayed
  , newDelayed
  , runDelayed
  , Eff'Delayed
  , wfDelayed
  ) where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Pipes
import Linear.V2
import SDLight.Types
import SDLight.Widgets.Core

data Layer
  = Layer
  { _layerWidth :: Int
  , _layerHeight :: Int
  , _layerTexture :: SDL.Texture
  }

makeLenses ''Layer

{-
source/target

<----> sx
                  V
| LT | CT | RT |  | sy
----------------  ^
|  L |  C |  R |
----------------
| LB | CB | RB |

-}

newLayer :: FilePath -> Int -> Int -> GameM Layer
newLayer path width height = do
  rend <- use renderer
  imgTexture <- lift $ SDL.loadTexture rend path
  resizeLayer imgTexture width height

resizeLayer :: SDL.Texture -> Int -> Int -> GameM Layer
resizeLayer imgTexture width height = do
  rend <- use renderer
  imgQuery <- SDL.queryTexture imgTexture
  let sx = fromEnum $ SDL.textureWidth imgQuery `div` 3
  let sy = fromEnum $ SDL.textureHeight imgQuery `div` 3
  let ssiz = V2 sx sy
  let sourceLoc = M.fromList [(fromEnum $ ix+iy*3, SDL.Rectangle (SDL.P $ V2 (sx*ix) (sy*iy)) ssiz) | ix <- [0..2], iy <- [0..2]]
  let targetLoc = M.fromList [(fromEnum $ ix+iy*3, SDL.Rectangle (SDL.P $ V2 (if ix/=2 then sx*ix else width-sx) (if iy/=2 then sy*iy else height-sy)) (V2 (if ix/=1 then sx else width-sx*2) (if iy/=1 then sy else height-sy*2))) | ix <- [0..2], iy <- [0..2]]

  emptyTexture <- lift $ SDL.createTexture rend SDL.ARGB8888 SDL.TextureAccessTarget (fmap toEnum $ V2 width height)
  SDL.rendererRenderTarget rend SDL.$= Just emptyTexture
  forM_ [ix+iy*3 | ix <- [0..2], iy <- [0..2]] $ \loc ->
    lift $ SDL.copy rend imgTexture (Just $ fmap toEnum $ sourceLoc M.! loc) (Just $ fmap toEnum $ targetLoc M.! loc)
  SDL.rendererRenderTarget rend SDL.$= Nothing
  
  return $ Layer width height emptyTexture

renderLayer :: Layer -> V2 Int -> GameM ()
renderLayer layer pos = do
  rend <- use renderer
  let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) (SDL.V2 (layer^.layerWidth) (layer^.layerHeight))
  lift $ SDL.copy rend (layer^.layerTexture) Nothing (Just $ fmap toEnum $ loc)

data Eff'Layer this m r where
  New'Layer :: FilePath -> Int -> Int -> Eff'Layer this GameM this
  Render'Layer :: this -> V2 Int -> Eff'Layer this GameM ()

wLayer :: Monad m => Widget (Eff'Layer Layer) m r
wLayer = await >>= \e -> case e of
  New'Layer path w h -> lift (newLayer path w h) >>= yield
  Render'Layer layer v -> lift (renderLayer layer v) >>= yield

-- Layered

newtype Layered a = Layered (a, Layer)

layered :: Lens' (Layered a) a
layered = lens (\(Layered (c,_)) -> c) (\(Layered (_,l)) c' -> Layered (c',l))

newLayered :: FilePath -> Int -> Int -> GameM a -> GameM (Layered a)
newLayered path w h initA = Layered <$> liftM2 (,) initA (newLayer path w h)

renderLayered :: Layered a -> V2 Int -> (a -> GameM ()) -> GameM ()
renderLayered (Layered (ma,layer)) pos k = renderLayer layer pos >> k ma

data Eff'Layered eff this (m :: * -> *) r where
  New'Layered :: FilePath -> Int -> Int -> eff this GameM this -> Eff'Layered eff this GameM (Layered this)
  Render'Layered :: (Layered this) -> V2 Int -> eff this GameM () -> Eff'Layered eff this GameM ()
  Lift'Layered :: eff this m r -> Eff'Layered eff this m r

wfLayered :: Widget (eff that) m r -> Widget (Eff'Layered eff that) m r
wfLayered widget = await >>= \case
  New'Layered path w h new ->
    for (yield new >-> widget) $ \t -> lift (newLayered path w h (return t)) >>= yield
  Render'Layered this v render ->
    for (yield render >-> widget) $ \() -> lift (renderLayered this v (\_ -> return ())) >> yield ()
  Lift'Layered op ->
    for (yield op >-> widget) yield

-- Delayed

data Delayed a
  = Delayed
  { _delayed :: a
  , _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delayed

newDelayed :: Int -> a -> Delayed a
newDelayed n ma = Delayed ma 0 n

runDelayed :: (a -> GameM a) -> Delayed a -> GameM (Delayed a)
runDelayed ma delay = do
  ma' <- if delay^.counter == 0 then ma (delay^.delayed) else return (delay^.delayed)
  return $ delay & delayed .~ ma'
                 & counter %~ (`mod` (delay^.delayCount)) . (+1)

data Eff'Delayed eff this (m :: * -> *) r where
  New'Delayed :: Int -> eff this GameM this -> Eff'Delayed eff this GameM (Delayed this)
  Run'Delayed :: Delayed this -> eff this GameM (this -> GameM this) -> Eff'Delayed eff this GameM (Delayed this)
  Lift'Delayed :: eff this m r -> Eff'Delayed eff this m r

wfDelayed :: Widget (eff that) m r -> Widget (Eff'Delayed eff that) m r
wfDelayed widget = await >>= \case
  New'Delayed n new ->
    for (yield new >-> widget) $ \t -> yield $ newDelayed n t
  Run'Delayed this run ->
    for (yield run >-> widget) $ \f -> lift (runDelayed f this) >>= yield
  Lift'Delayed op ->
    for (yield op >-> widget) yield

