module SDLight.Widgets.MessageLayer
  ( wMessageWriter
  , Op'MessageWriter
  , wMessageLayer
  , Op'MessageLayer
  ) where

import qualified SDL as SDL
import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Reflection
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Stylesheet
import SDLight.Widgets.Core
import SDLight.Widgets.Layer

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

wMessageWriter :: WidgetId -> [String] -> GameM (NamedWidget Op'MessageWriter)
wMessageWriter w = \mes -> wNamed (w </> WId "message-writer") . go <$> (new mes) where
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

type MessageLayer = (NamedWidget Op'Layer, Widget Op'Delay, NamedWidget Op'MessageWriter)

wMessageLayer :: Given StyleSheet => WidgetId -> SDL.Texture -> V2 Int -> [String] -> GameM (Widget Op'MessageLayer)
wMessageLayer = \w texture v mes -> go <$> new (w </> WId "message-layer") texture v mes where
  new w texture v mes = liftM3 (,,) (wLayer w texture v) (return $ wDelay 2) (wMessageWriter w mes)
  
  go :: MessageLayer -> Widget Op'MessageLayer
  go wm = Widget $
    (\(Op'Reset args) -> continue $ go $ wm & _2 ^%~ op'reset () & _3 ^%~ op'reset args)
    @> (\(Op'Render _ v) -> lift $ wm^._1^.op'render v >> wm^._3^.op'render v)
    @> (\Op'Run -> continueM $ fmap go $ (wm & _2 ^%%~ op'run) >>= run)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ wm & _3 ^%%~ op'handleEvent keys)
    @> (\Op'Switch -> bimapT (go . (\z -> wm & _3 .~ z)) id $ wm^._3^.op'switch)
    @> emptyUnion

  run wm | wm^._2^.op'getCounter == 0 = wm & _3 ^%%~ op'run
  run wm = return wm

