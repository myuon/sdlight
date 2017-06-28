{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Effector
  ( Op'Appear(..)
  , Op'Disappear(..)
  , Op'GetAlpha(..)
  , Op'IsAppeared(..)
  , Op'IsDisappeared(..)
  , Eff'Display
  , effDisplay
  , Transition(..)
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
  go EaseOut t = sin (t * pi / 2)
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

data Op'GetValue m r where
  Op'GetValue :: Op'GetValue Identity Double

type Op'Effector =
  [ Op'Reset '[]
  , Op'Run
  , Op'Start
  , Op'IsFinished
  , Op'GetValue
  ]

effector :: Transition -> Int -> Widget Op'Effector
effector = \tr n -> go (new tr n) where
  new :: Transition -> Int -> Effector
  new tr int = Effector 0 tr int NotReady (transit tr 0 int)

  go :: Effector -> Widget Op'Effector
  go eff = Widget $
    (\(Op'Reset _) -> continue go $ reset eff)
    @> (\Op'Run -> continueM go $ run eff)
    @> (\Op'Start -> continue go $ eff & _state .~ Running)
    @> (\Op'IsFinished -> finish $ eff^._state == Finished)
    @> (\Op'GetValue -> finish $ eff^.value)
    @> emptyUnion

  reset eff = eff
    & _state .~ NotReady
    & counter .~ 0
    & value .~ transit (eff^.function) 0 (eff^.interval) 

  run eff = case eff^._state of
    Running | eff^.counter >= eff^.interval -> return $ eff & _state .~ Finished
    Running -> return $ eff
      & counter +~ 1
      & value .~ transit (eff^.function) (eff^.counter) (eff^.interval) 
    _ -> return eff

data Op'Appear m r where
  Op'Appear :: Op'Appear Identity NoValue

data Op'Disappear m r where
  Op'Disappear :: Op'Disappear Identity NoValue

data Op'GetAlpha m r where
  Op'GetAlpha :: Op'GetAlpha Identity Double

data Op'IsAppeared m r where
  Op'IsAppeared :: Op'IsAppeared Identity Bool

data Op'IsDisappeared m r where
  Op'IsDisappeared :: Op'IsDisappeared Identity Bool

type Eff'Display =
  [ Op'Reset '[]
  , Op'Run
  , Op'Appear
  , Op'Disappear
  , Op'GetAlpha
  , Op'IsAppeared
  , Op'IsDisappeared
  ]

data EffDisplayeState
  = Appearing
  | Disappearing
  | Invisible
  | Visible
  deriving (Eq, Show)

effDisplay :: Transition -> Int -> Int -> Widget Eff'Display
effDisplay = \tr n1 n2 -> go Invisible (effector tr n1) (effector (Inverse tr) n2) where
  uncurry' f (a,b,c) = f a b c
  
  go :: EffDisplayeState -> Widget Op'Effector -> Widget Op'Effector -> Widget Eff'Display
  go st eff1 eff2 = Widget $
    (\(Op'Reset SNil) -> continue (uncurry' go) $ reset st eff1 eff2)
    @> (\Op'Run -> continueM (uncurry' go) $ run st eff1 eff2)
    @> (\Op'Appear -> continue (uncurry' go) (Appearing, eff1 @@. Op'Start, eff2))
    @> (\Op'Disappear -> continue (uncurry' go) (Disappearing, eff1, eff2 @@. Op'Start))
    @> (\Op'GetAlpha -> finish $ getAlpha st eff1 eff2)
    @> (\Op'IsAppeared -> finish $ st == Visible && eff1 @@! Op'IsFinished)
    @> (\Op'IsDisappeared -> finish $ st == Invisible && eff2 @@! Op'IsFinished)
    @> emptyUnion

  reset st eff1 eff2 = (Invisible, eff1 @@. Op'Reset SNil, eff2 @@. Op'Reset SNil)

  run :: EffDisplayeState -> Widget Op'Effector -> Widget Op'Effector -> GameM (EffDisplayeState, Widget Op'Effector, Widget Op'Effector)
  run st eff1 eff2 = case st of
    Appearing | eff1 @@! Op'IsFinished -> return (Visible, eff1, eff2)
    Appearing -> do
      eff1' <- eff1 @. Op'Run
      return (st, eff1', eff2)
    Disappearing | eff2 @@! Op'IsFinished -> return (Invisible, eff1, eff2)
    Disappearing -> do
      eff2' <- eff2 @. Op'Run
      return (st, eff1, eff2')
    _ -> return (st, eff1, eff2)

  getAlpha st eff1 eff2 = case st of
    Appearing -> eff1 @@! Op'GetValue
    Disappearing -> eff2 @@! Op'GetValue
    Invisible -> 0.0
    Visible -> 1.0

