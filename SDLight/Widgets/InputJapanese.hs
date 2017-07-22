{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.InputJapanese
  ( op'getText
  , Op'InputJapanese
  , wInputJapanese
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

data Op'GetText br m r where
  Op'GetText :: Op'GetText Value Identity String

op'getText :: Op'GetText ∈ xs => Getter (Widget xs) String
op'getText = _value' Op'GetText

type Op'InputJapanese =
  [ Op'Reset ()
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetText
  ]

data IJState = Selecting | Finished deriving (Eq, Show)

data InputJapanese
  = InputJapanese
  { _currentText :: String
  , _textLayer :: Widget Op'Layer
  , _letterLayer :: Widget Op'Layer
  , _pointer :: V2 Int
  , __state :: IJState
  }

makeLenses ''InputJapanese

wInputJapanese :: SDL.Texture -> GameM (Widget Op'InputJapanese)
wInputJapanese = \texture -> go <$> new texture where
  textLayerArea = V2 800 50
  letterLayerArea = V2 800 550
  
  new :: SDL.Texture -> GameM InputJapanese
  new texture =
    InputJapanese
    <$> return ""
    <*> wLayer texture (V2 800 50)
    <*> wLayer texture (V2 800 550)
    <*> return (V2 0 0)
    <*> return Selecting

  go :: InputJapanese -> Widget Op'InputJapanese
  go model = Widget $
    (\(Op'Reset _) -> continue $ go $ reset model)
    @> (\(Op'Render _ v) -> lift $ render model)
    @> (\Op'Run -> continue $ go model)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys model)
    @> (\Op'Switch -> (if model^._state == Finished then freeze' else continue) $ go model)
    @> (\Op'GetText -> finish $ model^.currentText)
    @> emptyUnion

  reset :: InputJapanese -> InputJapanese
  reset model = model & currentText .~ "" & pointer .~ V2 0 0 & _state .~ Selecting

  render :: InputJapanese -> GameM ()
  render model = do
    model^.textLayer^.op'render 0
    model^.letterLayer^.op'render 0

    when (model^.currentText /= "") $
      renders white [ translate (V2 15 15) $ shaded black $ text (model^.currentText) ]

    let V2 px py = model^.pointer
    let coordOfPos (V2 ix iy) = V2 ((9 - ix) * 75) (iy * 50) + V2 15 75
    forM_ (zip hiragana [0..]) $ \(hs,ix) -> do
      forM_ (zip hs [0..]) $ \(h,iy) -> do
        renders white [ translate (coordOfPos (V2 ix iy) + V2 40 0) $ shaded black $ text (return h) ]
    renders white [ translate (coordOfPos (V2 1 5) + V2 40 0) $ shaded black $ text "決定" ]
    
    renders white [ translate (coordOfPos (model^.pointer) + V2 15 0) $ shaded black $ text "▶" ]

  hiragana =
    [ "あいうえお"
    , "かきくけこ"
    , "さしすせそ"
    , "たちつてと"
    , "なにぬねの"
    , "はひふへほ"
    , "まみむめも"
    , "や　ゆ　よ"
    , "らりるれろ"
    , "わ　を　ん"
    ]

  handler :: M.Map SDL.Scancode Int -> InputJapanese -> GameM InputJapanese
  handler keys model
    | keys M.! SDL.ScancodeDown == 1 =
      if model^.pointer^._y == 4 then return $ model & pointer .~ V2 1 5
      else return $ model & pointer._y %~ (`mod` 6) . (+1)
    | keys M.! SDL.ScancodeUp == 1 =
      if model^.pointer^._y == 0 then return $ model & pointer .~ V2 1 5
      else return $ model & pointer._y %~ subtract 1
    | keys M.! SDL.ScancodeRight == 1 =
      if model^.pointer^._y == 5 then return $ model & pointer .~ V2 1 5
      else return $ model & pointer._x %~ (`mod` 10) . (+10) . (subtract 1)
    | keys M.! SDL.ScancodeLeft == 1 =
      if model^.pointer^._y == 5 then return $ model & pointer .~ V2 1 5
      else return $ model & pointer._x %~ (`mod` 10) . (+1)
    | keys M.! SDL.ScancodeZ == 1 =
      if model^.pointer == V2 1 5 then return $ model & _state .~ Finished
      else
        let ch = return $ hiragana !! (model^.pointer^._x) !! (model^.pointer^._y) in
        return $ model & currentText %~ if ch == " " then id else (++ ch)
    | otherwise = return model

