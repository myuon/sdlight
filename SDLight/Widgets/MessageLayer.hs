module SDLight.Widgets.MessageLayer
  ( wMessageWriter
  , Op'MessageWriter
  , wMessageLayer
  , Op'MessageLayer

  , MessageLayerConfig
  ) where

import qualified SDL as SDL
import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Reflection
import Data.Extensible
import Data.Default
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

type MessageWriterConfig =
  '[ "messages" >: [String]
  ]

instance Default (Config MessageWriterConfig) where
  def = Config $
    #messages @= []
    <: emptyRecord

wMessageWriter :: WConfig MessageWriterConfig -> GameM (NamedWidget Op'MessageWriter)
wMessageWriter (giveWid "message-writer" -> cfg) = wNamed (cfg ^. _Wrapped . #wix) . go <$> new where
  new = return $ reset (cfg ^. _Wrapped . #messages) $ MessageWriter [] 0 [] Typing 1

  go :: MessageWriter -> Widget Op'MessageWriter
  go mw = Widget $
    (\(Op'Reset mes') -> continue $ go $ reset mes' mw)
    @> (\(Op'Render _) -> lift $ render mw (getLocation cfg))
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

type MessageLayerConfig =
  [ "windowTexture" >: SDL.Texture
  , "size" >: V2 Int
  , "messages" >: [String]
  ]

instance Default (Config MessageLayerConfig) where
  def = Config
    $ #windowTexture @= error "not initialized"
    <: #size @= V2 800 200
    <: #messages @= []
    <: emptyRecord

wMessageLayer :: Given StyleSheet => WConfig MessageLayerConfig -> GameM (Widget Op'MessageLayer)
wMessageLayer (giveWid "message-layer" -> cfg) = go <$> new where
  new :: GameM MessageLayer
  new = liftM3 (,,)
    (wLayer $ Config $ #wix @= (cfg ^. _Wrapped . #wix) <: shrinkAssoc (cfg ^. _Wrapped))
    (return $ wDelay 2)
    (wMessageWriter $ cfgs _Wrapped $ #wix @= (cfg ^. _Wrapped . #wix) <: #messages @= (cfg ^. _Wrapped . #messages) <: emptyRecord)
  
  go :: MessageLayer -> Widget Op'MessageLayer
  go wm = Widget $
    (\(Op'Reset args) -> continue $ go $ wm & _2 ^%~ op'reset () & _3 ^%~ op'reset args)
    @> (\(Op'Render _) -> lift $ wm^._1^.op'renderAt (getLocation cfg) 1.0 >> wm^._3^.op'render)
    @> (\Op'Run -> continueM $ fmap go $ (wm & _2 ^%%~ op'run) >>= run)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ wm & _3 ^%%~ op'handleEvent keys)
    @> (\Op'Switch -> bimapT (go . (\z -> wm & _3 .~ z)) id $ wm^._3^.op'switch)
    @> emptyUnion

  run wm | wm^._2^.op'getCounter == 0 = wm & _3 ^%%~ op'run
  run wm = return wm

