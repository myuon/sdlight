{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Effector
  ( Op'Start(..)
  , Op'EffOpacity
  , effOpacity
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import qualified Data.Map as M
import Data.Functor.Sum
import Linear.V2
import Linear.Vector
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.Layer

data EffectorState
  = NotReady
  | Running
  | Finished
  deriving (Eq, Show)

data Transition
  = Linear
  | EaseOut
  | Inverse Transition
  deriving (Eq, Show)

transit :: Transition -> Int -> Int -> Double
transit tr current max = go tr (fromIntegral current / fromIntegral max) where
  go Linear t = t
  go EaseOut t = sin (t * pi / 4)
  go (Inverse tr) t = go tr (1-t)

data Effector
  = Effector
  { _counter :: Int
  , _function :: Transition
  , _interval :: Int
  , _estate :: EffectorState
  , _value :: Double
  }

makeLenses ''Effector

instance HasState Effector EffectorState where
  _state = estate

data Op'Start m r where
  Op'Start :: Op'Start Identity NoValue

type Op'EffOpacity =
  [ Op'Reset '[Transition, Int]
  , Op'Render
  , Op'Run
  , Op'Start
  , Op'IsFinished
  ]

effOpacity :: (Op'RenderAlpha : Op'EffOpacity) ⊆ xs
           => Transition -> Int -> Widget xs -> Widget (Op'EffOpacity ++ xs)
effOpacity = \tr n w -> go (new tr n) w where
  new :: Transition -> Int -> Effector
  new tr int = Effector 0 tr int NotReady 0

  go :: (Op'RenderAlpha : Op'EffOpacity) ⊆ xs
     => Effector -> Widget xs -> Widget (Op'EffOpacity ++ xs)
  go eff widget = Widget $
    (\(Op'Reset (tr :. (int :. SNil))) -> continue (go (eff & _state .~ NotReady & function .~ tr & interval .~ int)) widget)
    @> (\(Op'Render v) -> lift $ render v eff widget)
    @> (\Op'Run -> continueM (uncurry go) $ run eff widget)
    @> (\Op'Start -> continue (go (eff & _state .~ Running)) $ widget)
    @> (\Op'IsFinished -> finish $ widget @@!? Op'IsFinished && eff^._state == Finished)
    @> bimapEitherT (go eff) id . runWidget widget

  render v eff widget = case eff^._state of
    Running -> widget @!? Op'RenderAlpha (eff^.value) v
    _ -> widget @!? Op'Render v

  run eff widget = case eff^._state of
    Running -> do
      w' <- widget @. Op'Run
      return $ (eff, w')
      
    

