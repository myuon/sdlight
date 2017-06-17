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
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Object
import Control.Concurrent.MVar
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

data Op'NewLayer (m :: * -> *) r where
  Op'NewLayer :: FilePath -> V2 Int -> Op'NewLayer GameM Layer

wLayer :: FilePath -> V2 Int -> Eff '[Op'Render] GameM
wLayer path v = newLayer path v @@~
  (\(Op'Render v') -> get >>= \s -> lift $ renderLayer s v')
  @> emptyUnion

-- Layered

newtype Ref r v = Ref (MVar r,v)

instance Wrapped (Ref r v) where
  type Unwrapped (Ref r v) = (MVar r,v)
  _Wrapped' = iso (\(Ref m) -> m) Ref

_ref :: Lens' (Ref r v) (MVar r)
_ref = _Wrapped'._1

_refto :: Lens' (Ref r v) v
_refto = _Wrapped'._2


{-
newtype Layered a = Layered (a, Layer)

layered :: Lens' (Layered a) a
layered = lens (\(Layered (c,_)) -> c) (\(Layered (_,l)) c' -> Layered (c',l))

newLayered :: FilePath -> V2 Int -> GameM a -> GameM (Layered a)
newLayered path v initA = Layered <$> liftM2 (,) initA (newLayer path v)

renderLayered :: Layered a -> V2 Int -> (a -> GameM ()) -> GameM ()
renderLayered (Layered (ma,layer)) pos k = renderLayer layer pos >> k ma
-}

wfLayered :: (Op'Render ∈ xs, xs :<? Op'Render)
          => FilePath -> V2 Int -> Eff xs GameM -> Eff (xs :<@ Op'Render) GameM
wfLayered path v ef = Ref <$> ((,) <$> new ef <*> newLayer path v) @@~
  (\(Op'Render v') -> do
      Ref (s,r) <- get
      lift $ renderLayer s v'
      lift $ r .- Op'Render v'
  )
  @> _ --oplift ef

{-
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
-}

data Delay
  = Delay
  { _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

wfDelayed :: (Op'Run ∈ xs, xs :<? Op'Run) => Int -> Eff xs GameM -> Eff (xs :<@ Op'Run) GameM
wfDelayed n ef = (ef @:<@) $ Delay n 0 @~
  (\Op'Run -> do
      Delay c dc <- get
      when (c == 0) $ do
        lift $ ef @!! Op'Run
      
     )
  @> emptyUnion

{-
wfDelayed :: ( Lifts (xs :$ a)
             , Member (xs :$ a) (Op'New nargs a)
             , Member (xs :$ a) (Op'Run rargs a)
             )
          => Eff' xs a GameM
          -> Eff ( Op'New (Int : nargs) (Delayed a)
                 : Op'Run rargs (Delayed a)
                 : (Op'Lift :* (xs :$ a))) GameM
wfDelayed eff
  = (\(Op'New (n :. args)) -> newDelayed n <$> eff @! Op'New args)
  @>> (\(Op'Run args this) -> runDelayed (\a -> eff @! Op'Run args a) this)
  @>> oplift eff
-}

