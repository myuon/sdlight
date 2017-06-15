{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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
  , wLayer

  , Layered
  , layered
  , wfLayered

  , Delayed
  , delayed
  , wfDelayed
  ) where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Arrow (first, second)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.State.Strict
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

newLayer :: FilePath -> V2 Int -> GameM Layer
newLayer path (V2 width height) = do
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

data Op'NewLayer this r where
  Op'NewLayer :: FilePath -> V2 Int -> Op'NewLayer this this

data Op'RenderLayer this r where
  Op'RenderLayer :: V2 Int -> this -> Op'RenderLayer this ()

wLayer :: Eff [Op'NewLayer Layer, Op'RenderLayer Layer] GameM
wLayer
  = (\(Op'NewLayer path v) -> newLayer path v)
  @>> (\(Op'RenderLayer v layer) -> renderLayer layer v)
  @>> emptyEff

-- Layered

newtype Layered a = Layered (a, Layer)

layered :: Lens' (Layered a) a
layered = lens (\(Layered (c,_)) -> c) (\(Layered (_,l)) c' -> Layered (c',l))

newLayered :: FilePath -> V2 Int -> GameM a -> GameM (Layered a)
newLayered path v initA = Layered <$> liftM2 (,) initA (newLayer path v)

renderLayered :: Layered a -> V2 Int -> (a -> GameM ()) -> GameM ()
renderLayered (Layered (ma,layer)) pos k = renderLayer layer pos >> k ma

wfLayered :: ( Lifts (xs *: a)
             , Member (xs *: a) (Op'New a)
             , Member (xs *: a) (Op'Render a))
          => Eff' xs a GameM
          -> Eff (Op'NewLayer (Layered a) : Op'RenderLayer (Layered a) : (Op'Lift :* (xs *: a))) GameM
wfLayered eff
  = (\(Op'NewLayer path v) -> newLayered path v (eff @! Op'New))
  @>> (\(Op'RenderLayer v this) -> renderLayered this v (\a -> eff @! Op'Render a))
  @>> oplift eff

-- Delayed

data Delayed a
  = Delayed
  { _delayed :: a
  , _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delayed

data Op'NewDelay this r where
  Op'NewDelay :: Int -> Op'NewDelay this this

data Op'RunDelay this r where
  Op'RunDelay :: this -> Op'RunDelay this this

newDelayed :: Int -> a -> Delayed a
newDelayed n ma = Delayed ma 0 n

runDelayed :: (a -> GameM a) -> Delayed a -> GameM (Delayed a)
runDelayed ma delay = do
  ma' <- if delay^.counter == 0 then ma (delay^.delayed) else return (delay^.delayed)
  return $ delay & delayed .~ ma'
                 & counter %~ (`mod` (delay^.delayCount)) . (+1)

wfDelayed :: ( Lifts (xs *: a)
             , Member (xs *: a) (Op'New a)
             , Member (xs *: a) (Op'Run a)
             )
          => Eff' xs a GameM
          -> Eff (Op'NewDelay (Delayed a) : Op'RunDelay (Delayed a) : (Op'Lift :* (xs *: a))) GameM
wfDelayed eff
  = (\(Op'NewDelay n) -> newDelayed n <$> eff @! Op'New)
  @>> (\(Op'RunDelay this) -> runDelayed (\a -> eff @! Op'Run a) this)
  @>> oplift eff

