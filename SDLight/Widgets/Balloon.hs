{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Balloon
  ( Op'Balloon
  , Op'Fly(..)
  , wBalloon
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans (lift)
import qualified Data.Map as M
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Effector

data Op'Fly m r where
  Op'Fly :: Op'Fly Identity NoValue

type Op'Balloon =
  [ Op'Reset '[String]
  , Op'Render
  , Op'Run
  , Op'Fly
  , Op'IsFinished
  ]

data BalloonState
  = NotReady
  | Running
  | Finished
  deriving (Eq, Show)

data Balloon
  = Balloon
  { _balloonLayer :: Widget Op'Layer
  , _balloonText :: String
  , _eff :: Widget Eff'Display
  , _bstate :: BalloonState
  , _counter :: Int
  , _stayTime :: Int
  }

makeLenses ''Balloon

instance HasState Balloon BalloonState where
  _state = bstate

wBalloon :: FilePath -> String -> Int -> GameM (Widget Op'Balloon)
wBalloon = \path t stay -> go <$> new path t stay where
  new :: FilePath -> String -> Int -> GameM Balloon
  new path t stay =
    Balloon
    <$> wLayer path (V2 100 60)
    <*> return t
    <*> return (effDisplay EaseOut 40 40)
    <*> return NotReady
    <*> return 0
    <*> return stay

  go :: Balloon -> Widget Op'Balloon
  go model = Widget $
    (\(Op'Reset (t :. SNil)) -> continue go $ reset t model)
    @> (\(Op'Render v) -> lift $ render v model)
    @> (\Op'Run -> continueM go $ run model)
    @> (\Op'Fly -> continue go $ model & _state .~ Running & eff @%~ Op'Appear)
    @> (\Op'IsFinished -> finish $ model^._state == Finished && model^.eff @@! Op'IsDisappeared)
    @> emptyUnion

  reset :: String -> Balloon -> Balloon
  reset t model = model & counter .~ 0 & balloonText .~ t

  render :: V2 Int -> Balloon -> GameM ()
  render v model = do
    model^.balloonLayer @! Op'RenderAlpha (model^.eff @@! Op'GetAlpha) v
    when (model^.balloonText /= "") $
      renders white [ translate (v + V2 15 10) $ shaded black $ text (model^.balloonText) ]

  run :: Balloon -> GameM Balloon
  run model = case model^._state of
    Running | model^.counter >= model^.stayTime -> return $ model & _state .~ Finished & eff @%~ Op'Disappear
    Running | model^.eff @@! Op'IsAppeared -> return $ model & counter +~ 1
    _ -> model^.eff @. Op'Run >>= \e -> return $ model & eff .~ e
