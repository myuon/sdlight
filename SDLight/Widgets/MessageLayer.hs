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
  , __state :: MessageState
  , _maxLine :: Int
  }

makeLenses ''MessageWriter

type Op'MessageWriter =
  [ Op'Reset [String]
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  ]

wMessageWriter :: [String] -> GameM (Widget Op'MessageWriter)
wMessageWriter = \mes -> go <$> (new mes) where
  new mes = return $ reset mes $ MessageWriter [] 0 [] Typing 1

  go :: MessageWriter -> Widget Op'MessageWriter
  go mw = Widget $
    (\(Op'Reset mes') -> continue $ go $ reset mes' mw)
    @> (\(Op'Render _ v) -> lift $ render mw v)
    @> (\Op'Run -> continueM $ fmap go $ run mw)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys mw)
    @> (\Op'Switch -> (if mw^._state == Finished then freeze' else continue) $ go mw)
    @> emptyUnion

  reset :: [String] -> MessageWriter -> MessageWriter
  reset xs mes = mes
    & messages .~ drop 1 xs
    & currentMessages .~ (if xs /= [] then (take 1 xs) else ["initの引数がemptyです"])
    & _state .~ Typing
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

wMessageLayer :: SDL.Texture -> V2 Int -> [String] -> GameM (Widget Op'MessageLayer)
wMessageLayer = \texture v mes -> go <$> (wDelayed 2 <$> (wLayered texture v =<< wMessageWriter mes)) where
  go :: Widget (Op'Delayed (Op'Layered Op'MessageWriter)) -> Widget Op'MessageLayer
  go wm = Widget $
    (\(Op'Reset args) -> continue $ go $ wm ^. op'reset args)
    @> (\(Op'Render _ v) -> lift $ wm ^. op'render v)
    @> (\Op'Run -> continueM $ fmap go $ wm ^. op'run)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ wm ^. op'handleEvent keys)
    @> (\Op'Switch -> bimapT go id $ wm `call` Op'Switch)
    @> emptyUnion

