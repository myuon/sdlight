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
import Control.Monad.State.Strict
import Linear.V2
import SDLight.Util
import SDLight.Components
import SDLight.Types
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import Debug.Trace

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

type Op'MessageWriter =
  [ Op'Reset '[[String]]
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'IsFinished
  ]

wMessageWriter :: [String] -> GameM (Widget Op'MessageWriter)
wMessageWriter = \mes -> go <$> (new mes) where
  new mes = return $ reset mes $ MessageWriter [] 0 [] Typing 1

  go :: MessageWriter -> Widget Op'MessageWriter
  go mw = Widget $
    (\(Op'Reset (mes' :. SNil)) -> continue go $ reset mes' mw)
    @> (\(Op'Render v) -> lift $ render mw v)
    @> (\Op'Run -> continueM go $ run mw)
    @> (\(Op'HandleEvent keys) -> continueM go $ handler keys mw)
    @> (\Op'IsFinished -> finish $ mw^._state == Finished)
    @> emptyUnion

  reset :: [String] -> MessageWriter -> MessageWriter
  reset xs mes = mes
    & messages .~ drop 1 xs
    & currentMessages .~ (if xs /= [] then (take 1 xs) else ["initの引数がemptyです"])
    & mwstate .~ Typing
    & counter .~ V2 0 1

  render :: MessageWriter -> V2 Int -> GameM ()
  render mes pos =
    case mes^._state of
      Finished -> return ()
      _ -> do
        forM_ (zip [0..] $ take (mes^.counter^._y) $ mes^.currentMessages) $ \(i,m) -> do
          if i+1 == mes^.counter^._y
          then renders white [ translate (V2 (pos^._x) (pos^._y + 30*i)) $ shaded black $ text (take (mes^.counter^._x) m ++ " ") ]
          else renders white [ translate (V2 (pos^._x) (pos^._y + 30*i)) $ shaded black $ text m ]

  run :: MessageWriter -> GameM MessageWriter
  run mes =
    case mes^._state of
      Typing | mes^.counter^._y > mes^.maxLine ->
        return $ mes & _state .~ Waiting
      Typing -> do
        if mes^.counter^._x == length ((mes^.currentMessages) !! (mes^.counter^._y - 1))
          then return $ mes & counter .~ V2 0 (mes^.counter^._y+1)
          else return $ mes & counter . _x +~ 1
      _ -> return mes

  handler :: M.Map SDL.Scancode Int -> MessageWriter -> GameM MessageWriter
  handler keys mes
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


type Op'MessageLayer = Op'MessageWriter

data MessageLayer
  = MessageLayer
  { _wlayer :: Widget Op'Layer
  , _wdelay :: Widget Op'Delay
  , _wmes :: Widget Op'MessageWriter
  }

makeLenses ''MessageLayer

wMessageLayer :: FilePath -> V2 Int -> [String] -> GameM (Widget Op'MessageLayer)
wMessageLayer = \path v mes -> go <$> (new path v mes) where
  new path v mes =
    MessageLayer
    <$> wLayer path v
    <*> return (wDelay 2)
    <*> wMessageWriter mes

  go :: MessageLayer -> Widget Op'MessageLayer
  go wm = Widget $
    (\(Op'Reset args) -> continue go $ reset wm args)
    @> (\(Op'Render v) -> lift $ render v wm)
    @> (\Op'Run -> continueM go $ execStateT run wm)
    @> (\(Op'HandleEvent keys) -> continueM go $ execStateT (handler keys) wm)
    @> (\Op'IsFinished -> finish $ wm^.wmes @@! Op'IsFinished)
    @> emptyUnion

  reset wm args = wm & wmes @%~ Op'Reset args

  render v wm = do
    wm^.wlayer @! Op'Render v
    wm^.wmes @! Op'Render v

  run :: StateT MessageLayer GameM ()
  run = do
    delay <- use wdelay
    wdelay <~ lift (delay @. Op'Run)

    when (delay @@! Op'DelayRun) $ do
      mes <- use wmes
      wmes <~ lift (mes @. Op'Run)

  handler keys = do
    mes <- use wmes
    wmes <~ lift (mes @. Op'HandleEvent keys)
    
