{-# LANGUAGE TypeFamilies #-}
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
  [ "typing"
  , "waiting"
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

newMessageWriter :: [String] -> GameM MessageWriter
newMessageWriter mes = initMessageWriter mes $ MessageWriter [] 0 [] (inj @"typing" Proxy) 1

initMessageWriter :: [String] -> MessageWriter -> GameM MessageWriter
initMessageWriter xs mes =
  return $ mes & messages .~ (drop 1 xs)
               & currentMessages .~ (take 1 xs)
               & mwstate .~ inj @"typing" Proxy
               & counter .~ V2 0 1

runMessageWriter :: MessageWriter -> GameM MessageWriter
runMessageWriter mes =
  case symbolOf (mes^._state) of
    "typing" | mes^.counter^._y > mes^.maxLine ->
      return $ mes & _state .~ inj @"waiting" Proxy
    "typing" ->
      if mes^.counter^._x == length ((mes^.currentMessages) !! (mes^.counter^._y - 1))
      then return $ mes & counter .~ V2 0 (mes^.counter^._y+1)
      else return $ mes & counter . _x +~ 1
    _ -> return mes

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
        then renderShadedText (take (mes^.counter^._x) m ++ " ") white black (V2 (pos^._x) (pos^._y + 30*i))
        else renderShadedText m white black (V2 (pos^._x) (pos^._y + 30*i))

newtype MessageLayer = MessageLayer (Delayed (Layered MessageWriter))

newMessageLayer :: FilePath -> V2 Int -> [String] -> GameM MessageLayer
newMessageLayer path siz xs =
  MessageLayer <$> newDelayed 3 <$> (newLayered path (siz^._x) (siz^._y) $ newMessageWriter xs)

initMessageLayer :: [String] -> MessageLayer -> GameM MessageLayer
initMessageLayer xs (MessageLayer mes) = MessageLayer <$> runLensed mes (delayed.layered) (initMessageWriter xs)

runMessageLayer :: MessageLayer -> GameM MessageLayer
runMessageLayer (MessageLayer mes) =
  MessageLayer <$> runDelayed (\m -> runLensed m layered runMessageWriter) mes

handleMessageLayerEvent :: M.Map SDL.Scancode Int -> MessageLayer -> GameM MessageLayer
handleMessageLayerEvent keys (MessageLayer mes) =
  MessageLayer <$> handleEventLensed mes (delayed.layered) handleMessageWriterEvent keys

renderMessageLayer :: MessageLayer -> V2 Int -> GameM ()
renderMessageLayer (MessageLayer mes) p =
  let padding = V2 25 15 in
  renderLayered (mes^.delayed) p $ \m -> renderMessageWriter m (p + padding)

instance Wrapped MessageLayer where
  type Unwrapped MessageLayer = Delayed (Layered MessageWriter)
  _Wrapped' = iso (\(MessageLayer m) -> m) MessageLayer

instance HasState MessageLayer MessageState where
  _state = _Wrapped'.delayed.layered._state


