{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Balloon
  ( Op'Balloon
  , Op'IsShowing(..)
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

type Op'Balloon =
  [ Op'Reset '[String]
  , Op'Render
  , Op'Run
  , Op'Start
  , Op'IsFinished
  , Op'IsShowing
  ]

data Op'IsShowing m r where
  Op'IsShowing :: Op'IsShowing Identity Bool

data Balloon
  = Balloon
  { _balloonLayer :: Widget Op'Layer
  , _balloonText :: String

  , _counter :: Int
  , _popupTime :: Int
  }

makeLenses ''Balloon

wBalloon :: FilePath -> String -> Int -> Int -> GameM (Widget Op'Balloon)
wBalloon = \path t popup stay -> go <$> new path t popup stay where
  new :: FilePath -> String -> Int -> Int -> GameM Balloon
  new path t popup stay =
    Balloon
    <$> wLayer path (V2 100 60)
    <*> return t
    <*> return NotReady
    <*> return 0
    <*> return popup
    <*> return stay

  go :: Balloon -> Widget Op'Balloon
  go model = Widget $
    (\(Op'Reset (t :. SNil)) -> continue go $ reset t model)
    @> (\(Op'Render v) -> lift $ render v model)
    @> (\Op'Run -> continueM go $ run model)
    @> (\Op'Fly -> continue go $ model & _state .~ Display)
    @> (\Op'IsFinished -> finish $ model^._state == Finished)
    @> (\Op'IsShowing -> finish $ model^._state == Display)
    @> emptyUnion

  reset :: String -> Balloon -> Balloon
  reset t model = model & counter .~ 0 & balloonText .~ t

  render :: V2 Int -> Balloon -> GameM ()
  render v model = case model^._state of
    Display -> do
      model^.balloonLayer @!? Op'RenderAlpha (fromIntegral (model^.counter) / fromIntegral (model^.popupTime)) v
      when (model^.balloonText /= "") $
        renders white [ translate (v + V2 15 10) $ shaded black $ text (model^.balloonText) ]
    _ -> return ()

  run :: Balloon -> GameM Balloon
  run model = case model^._state of
    Display | model^.counter >= model^.popupTime + model^.stayTime ->
      return $ model & counter .~ 0 & _state .~ Finished
    Display -> return $ model & counter +~ 1
    _ -> return model

