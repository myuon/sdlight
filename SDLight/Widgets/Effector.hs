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
  , Op'Effector
  , effector

  , Eff'Display
  , effDisplay
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

type Op'Effector =
  [ Op'Reset '[]
  , Op'Run
  , Op'Start
  , Op'IsFinished
  ]

effector :: Transition -> Int -> Widget Op'Effector
effector = \tr n -> go (new tr n) where
  new :: Transition -> Int -> Effector
  new tr int = Effector 0 tr int NotReady 0

  go :: Effector -> Widget Op'Effector
  go eff = Widget $
    (\(Op'Reset _) -> continue go $ reset eff)
    @> (\Op'Run -> continueM go $ run eff)
    @> (\Op'Start -> continue go $ eff & _state .~ Running)
    @> (\Op'IsFinished -> finish $ eff^._state == Finished)
    @> emptyUnion

  reset eff = eff
    & _state .~ NotReady
    & counter .~ 0

  run eff = case eff^._state of
    Running | eff^.counter <= 0 -> return $ eff & _state .~ Finished
    Running -> return $ eff
      & counter -~ 1
      & value .~ transit (eff^.function) (eff^.interval) (eff^.counter)
    _ -> return eff

data Op'Appear m r where
  Op'Appear :: Op'Appear Identity NoValue

data Op'Disappear m r where
  Op'Disappear :: Op'Disappear Identity NoValue

type Eff'Display =
  [ Op'Reset '[]
  , Op'Run
  , Op'Appear
  , Op'Disappear
  , Op'IsFinished
  ]

data EffDisplayeState
  = Appearing
  | Disappearing
  | Other

effDisplay :: Transition -> Int -> Transition -> Int -> Widget Eff'Display
effDisplay = \tr1 n1 tr2 n2 -> go Other (effector tr1 n1) (effector tr2 n2) where
  uncurry' f (a,b,c) = f a b c
  
  go :: EffDisplayeState -> Widget Op'Effector -> Widget Op'Effector -> Widget Eff'Display
  go st eff1 eff2 = Widget $
    (\(Op'Reset SNil) -> continue (uncurry' go) $ (Other, eff1, eff2))
    @> (\Op'Run -> continueM (uncurry' go) $ run st eff1 eff2)
    @> (\Op'Appear -> continue (uncurry' go) (Appearing, eff1 @@. Op'Start, eff2))
    @> (\Op'Disappear -> continue (uncurry' go) (Disappearing, eff1, eff2 @@. Op'Start))
    @> (\Op'IsFinished -> finish $ eff2 @@!? Op'IsFinished)
    @> emptyUnion

  run :: EffDisplayeState -> Widget Op'Effector -> Widget Op'Effector -> GameM (EffDisplayeState, Widget Op'Effector, Widget Op'Effector)
  run st eff1 eff2 = case st of
    Appearing | eff1 @@!? Op'IsFinished -> return (Other, eff1, eff2)
    Appearing -> do
      eff1' <- eff1 @. Op'Run
      return (st, eff1', eff2)
    Disappearing | eff2 @@!? Op'IsFinished -> return (Other, eff1, eff2)
    Disappearing -> do
      eff2' <- eff2 @. Op'Run
      return (st, eff1, eff2')
    Other -> return (st, eff1, eff2)


