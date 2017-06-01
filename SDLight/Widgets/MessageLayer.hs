{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.MessageLayer where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import SDL.Compositor
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Linear.V2
import SDLight.Colors
import SDLight.Types
import SDLight.Text
import SDLight.Widgets.Layer

data MessageState
  = Waiting
  | Typing
  | Finished
  deriving (Eq, Show)

data MessageWriter
  = MessageWriter
  { _messages :: [String]
  , _counter :: V2 Int
  , _currentMessages :: [String]
  , _messageState :: MessageState
  , _maxLine :: Int
  }
  deriving (Eq, Show)

makeLenses ''MessageWriter

newMessageWriter :: [String] -> MessageWriter
newMessageWriter mes = MessageWriter (drop 1 mes) (V2 0 1) (take 1 mes) Typing 1

runMessageWriter :: MessageWriter -> MessageWriter
runMessageWriter mes =
  case mes^.messageState of
    Typing | mes^.counter^._y > mes^.maxLine ->
      mes & messageState .~ Waiting
    Typing ->
      if mes^.counter^._x == length ((mes^.currentMessages) !! (mes^.counter^._y - 1))
      then mes & counter .~ V2 0 (mes^.counter^._y+1)
      else mes & counter . _x +~ 1
    _ -> mes

handleMessageWriterEvent :: M.Map SDL.Scancode Int -> MessageWriter -> GameM MessageWriter
handleMessageWriterEvent keys mes
  | keys M.! SDL.ScancodeZ == 1 =
    case mes^.messageState of
      Waiting | mes^.messages == [] -> return mes
      Waiting | mes^.counter^._y > mes^.maxLine ->
        let (r,rest) = splitAt (mes^.maxLine) (mes^.messages) in
        return $ mes & counter .~ V2 0 1
                     & currentMessages .~ r
                     & messages .~ rest
                     & messageState .~ Typing
      Typing -> return $ mes & messageState .~ Waiting
                             & counter .~ V2 0 (mes^.counter^._y+1)
      _ -> return mes
  | otherwise = return mes
  
renderMessageWriter :: MessageWriter -> V2 Int -> GameM ()
renderMessageWriter mes pos =
  case mes^.messageState of
    Finished -> return ()
    _ -> do
      forM_ (zip [0..] $ take (mes^.counter^._y) $ mes^.currentMessages) $ \(i,m) -> do
        if i+1 == mes^.counter^._y
        then renderShadedText (take (mes^.counter^._x) m ++ " ") black white (V2 (pos^._x) (pos^._y + 30*i))
        else renderShadedText m black white (V2 (pos^._x) (pos^._y + 30*i))

