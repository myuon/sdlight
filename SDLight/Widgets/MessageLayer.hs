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
import Pipes hiding (Proxy)
import Linear.V2
import SDLight.Util
import SDLight.Components
import SDLight.Types
import SDLight.Widgets.Core
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
newMessageWriter mes = return $ initMessageWriter mes $ MessageWriter [] 0 [] (inj @"typing" Proxy) 1

initMessageWriter :: [String] -> MessageWriter -> MessageWriter
initMessageWriter xs mes =
  mes & messages .~ (drop 1 xs)
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
        then renders white [ translate (V2 (pos^._x) (pos^._y + 30*i)) $ shaded black $ text (take (mes^.counter^._x) m ++ " ") ]
        else renders white [ translate (V2 (pos^._x) (pos^._y + 30*i)) $ shaded black $ text m ]

data Eff'MessageWriter this m r where
  New'MessageWriter :: [String] -> Eff'MessageWriter this GameM this
  Reset'MessageWriter :: [String] -> this -> Eff'MessageWriter this GameM this
  Render'MessageWriter :: V2 Int -> this -> Eff'MessageWriter this GameM ()
  Step'MessageWriter :: this -> Eff'MessageWriter this GameM this
  HandleEvent'MessageWriter :: M.Map SDL.Scancode Int -> this -> Eff'MessageWriter this GameM this

wMessageWriter :: Widget (Eff'MessageWriter MessageWriter) m r
wMessageWriter = do
  await >>= \eff -> case eff of
    New'MessageWriter s -> lift (newMessageWriter s) >>= yield
    Reset'MessageWriter s mes -> yield (initMessageWriter s mes)
    Render'MessageWriter mes v -> lift (renderMessageWriter v mes) >>= yield
    Step'MessageWriter mes -> lift (runMessageWriter mes) >>= yield
    HandleEvent'MessageWriter keys mes -> lift (handleMessageWriterEvent keys mes) >>= yield

newtype MessageLayer = MessageLayer (Delayed (Layered MessageWriter))

{-
newMessageLayer :: FilePath -> V2 Int -> [String] -> GameM MessageLayer
newMessageLayer path siz xs =
  MessageLayer <$> newDelayed 3 <$> (newLayered path (siz^._x) (siz^._y) $ newMessageWriter xs)

initMessageLayer :: [String] -> MessageLayer -> MessageLayer
initMessageLayer xs (MessageLayer mes) = MessageLayer $ mes & delayed.layered %~ initMessageWriter xs

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
-}

instance Wrapped MessageLayer where
  type Unwrapped MessageLayer = Delayed (Layered MessageWriter)
  _Wrapped' = iso (\(MessageLayer m) -> m) MessageLayer

instance HasState MessageLayer MessageState where
  _state = _Wrapped'.delayed.layered._state

data Eff'MessageLayer this m r where
  New'MessageLayer :: FilePath -> V2 Int -> [String] -> Eff'MessageLayer this GameM this
  Reset'MessageLayer :: [String] -> this -> Eff'MessageLayer this GameM this
  Render'MessageLayer :: V2 Int -> this -> Eff'MessageLayer this GameM ()
  Step'MessageLayer :: this -> Eff'MessageLayer this GameM this
  HandleEvent'MessageLayer :: M.Map SDL.Scancode Int -> this -> Eff'MessageLayer this GameM this

-- Map'Delayed :: (this -> eff this m r) -> Delayed this -> Eff'Delayed eff this m (Delayed r)
-- Map'Layered :: (this -> eff this m r) -> Layered this -> Eff'Layered eff this m (Layered r)

-- Reset'MessageWriter xs : MW -> Eff MW m ME
-- Map'Layred (..) : Layered MW -> Eff eff MW m (Layered MW)
-- Map'Delayed .. : 

wMessageLayer :: Widget (Eff'MessageLayer MessageLayer) m r
wMessageLayer = await >>= \case
  New'MessageLayer path v s ->
    for (yield (New'Delayed 2 $ New'Layered path v $ New'MessageWriter s) >-> widget) $ yield . MessageLayer
  Reset'MessageLayer xs this ->
    for (yield (Map'Delayed (Map'Layered (Reset'MessageWriter xs)) (this^._Wrapped')) >-> widget) $ \w -> yield $ w^._Unwrapped'
  Render'MessageLayer v this ->
    let padding = V2 25 15 in
    for (yield (Map'Delayed (Render'Layered v (Render'MessageWriter (v + padding))) (this^._Wrapped' & delayed .~ ())) >-> widget) $ \_ -> yield ()
  Step'MessageLayer this ->
    for (yield (Step'Delayed (this^._Wrapped') $ Map'Layered (this^._Wrapped'^.delayed) $ Step'MessageWriter $ this^._Wrapped'^.delayed^.layered) >-> widget) $ \w -> yield $ MessageLayer w
  HandleEvent'MessageLayer keys this ->
    for (yield (Map'Delayed (this^._Wrapped') $ Map'Layered (this^._Wrapped'^.delayed) $ HandleEvent'MessageWriter keys (this^._Wrapped'^.delayed^.layered)) >-> widget) $ yield . MessageLayer
  where
    widget :: Widget (Eff'Delayed (Eff'Layered Eff'MessageWriter) MessageWriter) m r
    widget = wfDelayed $ wfLayered $ wMessageWriter

