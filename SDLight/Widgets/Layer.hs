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
  , Op'Layer
  , wfLayered
  , Op'Layered
  , wfDelayed
  , Op'Delayed
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
import Control.Monad.Trans.Either
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

type Op'Layer = '[Op'Render]

wLayer :: FilePath -> V2 Int -> GameM (Widget Op'Layer)
wLayer path v = go <$> newLayer path v where
  go :: Layer -> Widget '[Op'Render]
  go layer = Widget $
    (\(Op'Render v) -> lift $ renderLayer layer v)
    @> emptyUnion

-- Layered

type Op'Layered xs = xs :<<: Op'Layer

wfLayered :: (Lifting xs, Op'Render ∈ xs)
          => FilePath -> V2 Int -> Widget xs -> GameM (Widget (Op'Layered xs))
wfLayered path v w = liftM2 go (newLayer path v) (return $ wlift w) where
  go :: (Lifting xs, Op'Render ∈ xs)
     => Layer -> Widget (Lifted xs) -> Widget (Op'Layered xs)
  go layer widget = Widget $
    (\(Op'Render v) -> do
      lift $ renderLayer layer v
      lift $ wunlift widget @!? Op'Render v
    ) @> bimapEitherT (go layer) id . runWidget widget

-- Delayed

data Delay
  = Delay
  { _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delay

type Op'Delayed xs = xs :<<: '[Op'Run]

wfDelayed :: (Lifting xs, Op'Run ∈ xs)
          => Int -> Widget xs -> Widget (Op'Delayed xs)
wfDelayed n w = go (Delay 0 n) (wlift w) where
  go :: (Lifting xs, Op'Run ∈ xs)
     => Delay -> Widget (Lifted xs) -> Widget (Op'Delayed xs)
  go delay widget = Widget $
    (\Op'Run -> do
      let delay' = delay & counter %~ (`mod` (delay^.delayCount)) . (+1)
      if delay'^.counter == 0
        then do
          w' <- lift $ wunlift widget @. Op'Run
          continueM (go delay') $ return $ wlift w'
        else left $ go delay' widget
    ) @> bimapEitherT (go delay) id . runWidget widget


