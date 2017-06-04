{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.MessageLayer where

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
import SDLight.Types
import SDLight.Text
import SDLight.Widgets.Layer

type MessageState
  = SymbolOf
  [ "init"
  , "waiting"
  , "typing"
  , "finished"
  ]

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

newMessageWriter :: [String] -> MessageWriter
newMessageWriter mes = MessageWriter (drop 1 mes) (V2 0 1) (take 1 mes) (inj @"init" Proxy) 1

runMessageWriter :: MessageWriter -> MessageWriter
runMessageWriter mes =
  case symbolOf (mes^._state) of
    "typing" | mes^.counter^._y > mes^.maxLine ->
      mes & _state .~ inj @"waiting" Proxy
    "typing" ->
      if mes^.counter^._x == length ((mes^.currentMessages) !! (mes^.counter^._y - 1))
      then mes & counter .~ V2 0 (mes^.counter^._y+1)
      else mes & counter . _x +~ 1
    _ -> mes

handleMessageWriterEvent :: M.Map SDL.Scancode Int -> MessageWriter -> GameM MessageWriter
handleMessageWriterEvent keys mes
  | keys M.! SDL.ScancodeZ == 1 =
    case symbolOf (mes^._state) of
      "waiting" | mes^.messages == [] -> return $ mes & _state .~ inj @"finished" Proxy
      "waiting" | mes^.counter^._y > mes^.maxLine ->
        let (r,rest) = splitAt (mes^.maxLine) (mes^.messages) in
        return $ mes & counter .~ V2 0 1
                     & currentMessages .~ r
                     & messages .~ rest
                     & _state .~ inj @"typing" Proxy
      "typing" -> return $ mes & _state .~ inj @"waiting" Proxy
                             & counter .~ V2 0 (mes^.counter^._y+1)
      _ -> return mes
  | otherwise = return mes
  
renderMessageWriter :: MessageWriter -> V2 Int -> GameM ()
renderMessageWriter mes pos =
  case symbolOf (mes^._state) of
    "finished" -> return ()
    _ -> do
      forM_ (zip [0..] $ take (mes^.counter^._y) $ mes^.currentMessages) $ \(i,m) -> do
        if i+1 == mes^.counter^._y
        then renderShadedText (take (mes^.counter^._x) m ++ " ") black white (V2 (pos^._x) (pos^._y + 30*i))
        else renderShadedText m black white (V2 (pos^._x) (pos^._y + 30*i))

