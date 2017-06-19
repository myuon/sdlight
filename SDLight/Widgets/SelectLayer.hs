{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.SelectLayer
  ( Op'SelectLayer
  , wSelectLayer
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans (lift)
import qualified Data.Map as M
import Linear.V2
import SDLight.Types
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Selector
import SDLight.Components
import SDLight.Util

type Op'SelectLayer =
  [ Op'Reset '[]
  , Op'Render
  , Op'HandleEvent
  , Op'GetSelecting
  , Op'IsFinished
  ]

wSelectLayer :: FilePath -> FilePath -> V2 Int -> [String] -> Int -> GameM (Widget Op'SelectLayer)
wSelectLayer = \win cur v labels num -> liftM3 go (wLayer win v) (wLayer cur (V2 (v^._x - 20) 30)) (return $ wSelector labels num) where
  go :: Widget Op'Layer -> Widget Op'Layer -> Widget Op'Selector -> Widget Op'SelectLayer
  go wwindow wcursor wsel = Widget $
    (\(Op'Reset args) -> continue (go wwindow wcursor) $ reset args wsel)
    @> (\(Op'Render v) -> lift $ render wwindow wcursor wsel v)
    @> (\(Op'HandleEvent keys) -> continueM (go wwindow wcursor) $ handler keys wsel)
    @> (\Op'GetSelecting -> finish $ wsel @@!? Op'GetSelecting)
    @> (\Op'IsFinished -> finish $ wsel @@!? Op'IsFinished)
    @> emptyUnion

  reset :: Seq '[] -> Widget Op'Selector -> Widget Op'Selector
  reset args wsel = wsel @@. Op'Reset args

  render :: Widget Op'Layer -> Widget Op'Layer -> Widget Op'Selector -> V2 Int -> GameM ()
  render wwindow wcursor wsel v = do
    wwindow @!? Op'Render v
    (wsel @!?) $ Op'RenderBy $ \label i selecting focused -> do
      when focused $ do
        wcursor @!? Op'Render (v + V2 10 (20+30*i))

      let color = if selecting then red else white
      renders color $
        [ translate (v + V2 (20+5) (20+30*i)) $ shaded black $ text label
        ]

  handler :: M.Map SDL.Scancode Int -> Widget Op'Selector -> GameM (Widget Op'Selector)
  handler keys wsel = wsel @. Op'HandleEvent keys

