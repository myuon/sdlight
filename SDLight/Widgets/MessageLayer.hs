{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.MessageLayer
  ( wMessageWriter
  , Op'MessageWriter
  , wMessageLayer
  , Op'MessageLayer
  ) where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Proxy
import Control.Lens
import Control.Monad
import Control.Monad.State
import Linear.V2
import SDLight.Util
import SDLight.Components
import SDLight.Types
import SDLight.Widgets.Core
import SDLight.Widgets.Layer

data MessageState = Typing | Waiting | Finished
  deriving (Eq, Show)

data MessageWriter
  = MessageWriter
  { _messages :: [String]
  , _counter :: V2 Int
  , _currentMessages :: [String]
  , _mwstate :: MessageState
  , _maxLine :: Int
  }

makeLenses ''MessageWriter

instance HasState MessageWriter MessageState where
  _state = mwstate

newMessageWriter :: [String] -> GameM MessageWriter
newMessageWriter mes = return $ initMessageWriter mes $ MessageWriter [] 0 [] Typing 1

initMessageWriter :: [String] -> MessageWriter -> MessageWriter
initMessageWriter xs mes =
  mes & messages .~ (drop 1 xs)
      & currentMessages .~ (take 1 xs)
      & mwstate .~ Typing
      & counter .~ V2 0 1

runMessageWriter :: MessageWriter -> GameM MessageWriter
runMessageWriter mes =
  case mes^._state of
    Typing | mes^.counter^._y > mes^.maxLine ->
      return $ mes & _state .~ Waiting
    Typing ->
      if mes^.counter^._x == length ((mes^.currentMessages) !! (mes^.counter^._y - 1))
      then return $ mes & counter .~ V2 0 (mes^.counter^._y+1)
      else return $ mes & counter . _x +~ 1
    _ -> return mes

handleMessageWriterEvent :: M.Map SDL.Scancode Int -> MessageWriter -> GameM MessageWriter
handleMessageWriterEvent keys mes
  | keys M.! SDL.ScancodeZ == 1 =
    case mes^._state of
      Waiting | mes^.messages == [] -> return $ mes & _state .~ Finished
      Waiting | mes^.counter^._y > mes^.maxLine ->
        let (r,rest) = splitAt (mes^.maxLine) (mes^.messages) in
        return $ mes & counter .~ V2 0 1
                     & currentMessages .~ r
                     & messages .~ rest
                     & _state .~ Typing
      Typing -> return $ mes & _state .~ Waiting
                             & counter .~ V2 0 (mes^.counter^._y+1)
      _ -> return mes
  | otherwise = return mes

renderMessageWriter :: MessageWriter -> V2 Int -> GameM ()
renderMessageWriter mes pos =
  case mes^._state of
    Finished -> return ()
    _ -> do
      forM_ (zip [0..] $ take (mes^.counter^._y) $ mes^.currentMessages) $ \(i,m) -> do
        if i+1 == mes^.counter^._y
        then renders white [ translate (V2 (pos^._x) (pos^._y + 30*i)) $ shaded black $ text (take (mes^.counter^._x) m ++ " ") ]
        else renders white [ translate (V2 (pos^._x) (pos^._y + 30*i)) $ shaded black $ text m ]

type Op'MessageWriter = [Op'Reset '[[String]], Op'Render, Op'Run, Op'HandleEvent, Op'IsFinished]

wMessageWriter :: [String] -> GameM (Widget Op'MessageWriter)
wMessageWriter mes = go <$> (newMessageWriter mes) where
  go :: MessageWriter -> Widget Op'MessageWriter
  go mw = Widget $
    (\(Op'Reset (mes' :. _)) -> continue go $ initMessageWriter mes' mw)
    @> (\(Op'Render v) -> lift $ renderMessageWriter mw v)
    @> (\Op'Run -> continueM go $ runMessageWriter mw)
    @> (\(Op'HandleEvent keys) -> continueM go $ handleMessageWriterEvent keys mw)
    @> (\Op'IsFinished -> finish $ mw^._state == Finished)
    @> emptyUnion

--

type Op'MessageLayer = Op'MessageWriter

wMessageLayer :: FilePath -> V2 Int -> [String]
              -> GameM (Widget Op'MessageLayer)
wMessageLayer path v mes = go <$> (wfDelayed 2 . oprun <$> (wfLayered path v =<< wMessageWriter mes)) where
  oprun :: Widget (Op'Layered Op'MessageWriter) -> Widget (Op'Run : Op'Layered Op'MessageWriter)
  oprun w = (\Op'Run -> continueM oprun $ (w @. Op'Lift Op'Run)) @?> w
  
  go :: Widget (Op'Delayed (Op'Run : Op'Layered Op'MessageWriter)) -> Widget Op'MessageLayer
  go widget = Widget $
    (\(Op'Reset args) -> continueM go $ widget @. Op'Lift (Op'Lift (Op'Reset args)))
    @> (\(Op'Render v) -> lift $ widget @!? Op'Lift (Op'Render v))
    @> (\Op'Run -> continueM go $ widget @. Op'Run)
    @> (\(Op'HandleEvent keys) -> continueM go $ widget @. Op'Lift (Op'Lift (Op'HandleEvent keys)))
    @> (\Op'IsFinished -> finish $ widget @@!? Op'Lift (Op'Lift Op'IsFinished))
    @> emptyUnion

