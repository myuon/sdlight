{-# LANGUAGE UndecidableInstances #-}
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
import Data.Extensible
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Stylesheet
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Animated

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

makeOp "IsWaiting" [t| _ Value Identity Bool |]

type Op'MessageWriter =
  [ Op'Reset [String]
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'IsWaiting
  ]

instance Conf "message_writer" where
  type Required "message_writer" = '[]
  type Optional "message_writer" =
    '[ "messages" >: [String]
     ]
  
  def =
    #messages @= []
    <: emptyRecord

wMessageWriter :: Given StyleSheet => WConfig "message_writer" -> GameM (NamedWidget Op'MessageWriter)
wMessageWriter (wconf #message_writer -> ViewWConfig wix req opt) = wNamed wix . go <$> new where
  new = return $ reset (opt ^. #messages) $ MessageWriter [] 0 [] Typing 1

  go :: MessageWriter -> Widget Op'MessageWriter
  go mw = Widget $
    (\(Op'Reset mes') -> continue $ go $ reset mes' mw)
    @> (\(Op'Render _) -> lift $ render mw (given ^. wlocation wix))
    @> (\Op'Run -> continueM $ fmap go $ run mw)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys mw)
    @> (\Op'Switch -> (if mw^._state == Finished then freeze' else continue) $ go mw)
    @> (\Op'IsWaiting -> finish $ mw ^. _state == Waiting)
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


type Op'MessageLayer =
  [ Op'Reset [String]
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  ]

data MessageLayer
  = MessageLayer
  { _windowLayer :: NamedWidget Op'Layer
  , _delay :: Widget Op'Delay
  , _messager :: NamedWidget Op'MessageWriter
  , _clickwait :: Widget Op'Animated
  }

makeLenses ''MessageLayer

instance Conf "message_layer" where
  type Required "message_layer" =
    [ "windowTexture" >: SDL.Texture
    , "clickwaitConfig" >: Record (Required "animated")
    ]
  type Optional "message_layer" =
    [ "size" >: V2 Int
    , "messages" >: [String]
    ]
  
  def =
    #size @= V2 800 150
    <: #messages @= []
    <: emptyRecord

wMessageLayer :: Given StyleSheet => WConfig "message_layer" -> GameM (Widget Op'MessageLayer)
wMessageLayer (wconf #message_layer -> ViewWConfig wix req opt) = go <$> new where
  new :: GameM MessageLayer
  new = liftM4 MessageLayer
    (wLayer $ conf @"layer" wix (#windowTexture @= (req ^. #windowTexture) <: #size @= (opt ^. #size) <: emptyRecord) (def @"layer"))
    (return $ wDelay 2)
    (wMessageWriter $ conf @"message_writer" wix emptyRecord (shrinkAssoc opt))
    (wAnimated $ conf @"animated" wix (req ^. #clickwaitConfig) (def @"animated"))
  
  go :: MessageLayer -> Widget Op'MessageLayer
  go wm = Widget $
    (\(Op'Reset args) -> continue $ go $ wm & delay ^%~ op'reset () & messager ^%~ op'reset args)
    @> (\(Op'Render _) -> lift $ render wm)
    @> (\Op'Run -> continueM $ fmap go $ (wm & delay ^%%~ op'run) >>= run >>= runClickwait)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ wm & messager ^%%~ op'handleEvent keys)
    @> (\Op'Switch -> bimapT (go . (\z -> wm & messager .~ z)) id $ wm^.messager^.op'switch)
    @> emptyUnion

  render wm = do
    wm^.windowLayer^.op'render
    wm^.messager^.op'render

    when (wm^.messager^.op'isWaiting) $ wm^.clickwait^.op'render

  run wm | wm^.delay^.op'getCounter == 0 = wm & messager ^%%~ op'run
  run wm = return wm

  runClickwait wm | wm^.messager^.op'isWaiting = wm & clickwait ^%%~ op'run
  runClickwait wm = return wm
    

