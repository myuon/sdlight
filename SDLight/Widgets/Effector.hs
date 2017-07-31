module SDLight.Widgets.Effector
  ( op'appear
  , op'disappear
  , op'getAlpha
  , op'isAppeared
  , op'isDisappeared
  , Eff'Display

  , op'start
  , op'getValue
  , effector
  , Op'Effector

  , effDisplay
  , Transition(..)
  ) where

import Control.Lens
import SDLight.Types
import SDLight.Widgets.Core

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
  , __state :: EffectorState
  , _value :: Double
  }

makeLenses ''Effector

makeOp "Start" [t| _ Self Identity () |]
makeOp "GetValue" [t| _ Value Identity Double |]

type Op'Effector =
  [ Op'Reset ()
  , Op'Run
  , Op'Start
  , Op'Switch
  , Op'GetValue
  ]

effector :: Transition -> Int -> Widget Op'Effector
effector = \tr n -> go (new tr n) where
  new :: Transition -> Int -> Effector
  new tr int = Effector 0 tr int NotReady (transit tr 0 int)

  go :: Effector -> Widget Op'Effector
  go eff = Widget $
    (\(Op'Reset _) -> continue $ go $ reset eff)
    @> (\Op'Run -> continueM $ fmap go $ run eff)
    @> (\Op'Start -> continue $ go $ eff & _state .~ Running)
    @> (\Op'Switch -> (if eff^._state == Finished then freeze' else continue) $ go eff)
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

data Op'Appear br m r where
  Op'Appear :: Op'Appear Self Identity a

data Op'Disappear br m r where
  Op'Disappear :: Op'Disappear Self Identity a

data Op'GetAlpha br m r where
  Op'GetAlpha :: Op'GetAlpha Value Identity Double

data Op'IsAppeared br m r where
  Op'IsAppeared :: Op'IsAppeared Value Identity Bool

data Op'IsDisappeared br m r where
  Op'IsDisappeared :: Op'IsDisappeared Value Identity Bool

op'appear :: Op'Appear ∈ xs => Getter (Widget xs) (Widget xs)
op'appear = _self' Op'Appear

op'disappear :: Op'Disappear ∈ xs => Getter (Widget xs) (Widget xs)
op'disappear = _self' Op'Disappear

op'getAlpha :: Op'GetAlpha ∈ xs => Getter (Widget xs) Double
op'getAlpha = _value' Op'GetAlpha

op'isAppeared :: Op'IsAppeared ∈ xs => Getter (Widget xs) Bool
op'isAppeared = _value' Op'IsAppeared

op'isDisappeared :: Op'IsDisappeared ∈ xs => Getter (Widget xs) Bool
op'isDisappeared = _value' Op'IsDisappeared

type Eff'Display =
  [ Op'Reset ()
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
    (\(Op'Reset _) -> continue $ reset st eff1 eff2)
    @> (\Op'Run -> continueM $ fmap (uncurry' go) $ run st eff1 eff2)
    @> (\Op'Appear -> continue $ go Appearing (eff1 ^. op'start) eff2)
    @> (\Op'Disappear -> continue $ go Disappearing eff1 (eff2 ^. op'start))
    @> (\Op'GetAlpha -> finish $ getAlpha st eff1 eff2)
    @> (\Op'IsAppeared -> finish $ st == Visible && op'isFreeze eff1 op'switch)
    @> (\Op'IsDisappeared -> finish $ st == Invisible && op'isFreeze eff2 op'switch)
    @> emptyUnion

  reset st eff1 eff2 = go Invisible (eff1 ^. op'reset ()) (eff2 ^. op'reset ())

  run :: EffDisplayeState -> Widget Op'Effector -> Widget Op'Effector -> GameM (EffDisplayeState, Widget Op'Effector, Widget Op'Effector)
  run st eff1 eff2 = case st of
    Appearing | op'isFreeze eff1 op'switch -> return (Visible, eff1, eff2)
    Appearing -> do
      eff1' <- eff1 ^. op'run
      return (st, eff1', eff2)
    Disappearing | op'isFreeze eff2 op'switch -> return (Invisible, eff1, eff2)
    Disappearing -> do
      eff2' <- eff2 ^. op'run
      return (st, eff1, eff2')
    _ -> return (st, eff1, eff2)

  getAlpha st eff1 eff2 = case st of
    Appearing -> eff1 ^. _value' Op'GetValue
    Disappearing -> eff2 ^. _value' Op'GetValue
    Invisible -> 0.0
    Visible -> 1.0
