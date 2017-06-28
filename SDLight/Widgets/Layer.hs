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
  ( wLayer
  , Op'Layer
  , Op'RenderAlpha(..)

  , wLayered
  , Op'Layered

  , wDelayed
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
import Data.Functor.Sum
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
  layer <- resizeLayer imgTexture width height
  SDL.destroyTexture imgTexture
  return layer

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
  SDL.textureBlendMode emptyTexture SDL.$= SDL.BlendAlphaBlend
  
  SDL.rendererRenderTarget rend SDL.$= Just emptyTexture
  forM_ [ix+iy*3 | ix <- [0..2], iy <- [0..2]] $ \loc ->
    lift $ SDL.copy rend imgTexture (Just $ fmap toEnum $ sourceLoc M.! loc) (Just $ fmap toEnum $ targetLoc M.! loc)
  SDL.rendererRenderTarget rend SDL.$= Nothing
    
  return $ Layer width height emptyTexture

renderLayer :: Layer -> V2 Int -> Double -> GameM ()
renderLayer layer pos alpha = do
  rend <- use renderer
  let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) (SDL.V2 (layer^.layerWidth) (layer^.layerHeight))

  alpha0 <- SDL.get $ SDL.textureAlphaMod (layer^.layerTexture)
  SDL.textureAlphaMod (layer^.layerTexture) SDL.$= (floor $ alpha * 255)
  lift $ SDL.copy rend (layer^.layerTexture) Nothing (Just $ fmap toEnum $ loc)
  SDL.textureAlphaMod (layer^.layerTexture) SDL.$= alpha0

type Op'Layer =
  [ Op'Render
  , Op'RenderAlpha
  ]

data Op'RenderAlpha m r where
  Op'RenderAlpha :: Double -> V2 Int -> Op'RenderAlpha GameM ()

wLayer :: FilePath -> V2 Int -> GameM (Widget Op'Layer)
wLayer path v = go <$> newLayer path v where
  go :: Layer -> Widget Op'Layer
  go layer = Widget $
    (\(Op'Render v) -> lift $ renderLayer layer v 1.0)
    @> (\(Op'RenderAlpha alpha v) -> lift $ renderLayer layer v alpha)
    @> emptyUnion

-- Layered

type Op'Layered xs = Op'Layer ++ xs

wLayered :: Op'Render ∈ xs => FilePath -> V2 Int -> Widget xs -> GameM (Widget (Op'Layered xs))
wLayered = \path v w -> liftM2 go (wLayer path v) (return w) where
  go :: Op'Render ∈ xs => Widget Op'Layer -> Widget xs -> Widget (Op'Layered xs)
  go wlayer wx = override (go wlayer) wx $ 
    (\(Op'Render v) -> InL $ lift $ renderAlpha 1.0 v wlayer wx)
    @> (\(Op'RenderAlpha alpha v) -> InL $ lift $ renderAlpha alpha v wlayer wx)
    @> InR

  renderAlpha :: Op'Render ∈ xs => Double -> V2 Int -> Widget Op'Layer -> Widget xs -> GameM ()
  renderAlpha alpha v wlayer wx = do
    wlayer @! Op'RenderAlpha alpha v
    wx @! Op'Render v

-- Delayed

data Delay
  = Delay
  { _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delay

type Op'Delayed xs = Op'Run : xs

wDelayed :: Op'Run ∈ xs => Int -> Widget xs -> Widget (Op'Delayed xs)
wDelayed = \n w -> go (Delay 0 n) w where
  go :: Op'Run ∈ xs => Delay -> Widget xs -> Widget (Op'Delayed xs)
  go delay widget = override (go delay) widget $
    (\Op'Run -> InL $ continueM (uncurry go) $ execStateT run (delay,widget))
    @> InR

  run :: Op'Run ∈ xs => StateT (Delay, Widget xs) GameM ()
  run = do
    c <- use $ _1.counter
    when (c == 0) $ do
      w <- use _2
      _2 <~ lift (w @. Op'Run)

    d <- use $ _1.delayCount
    _1.counter %= (`mod` d) . (+1)
  

