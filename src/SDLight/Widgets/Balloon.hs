module SDLight.Widgets.Balloon
  ( Op'Balloon
  , op'fly
  , wBalloon
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans (lift)
import Data.Reflection
import Data.Extensible
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Stylesheet
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Effector

makeOp "Fly" [t| _ Self Identity () |]

type Op'Balloon =
  [ Op'Reset String
  , Op'Render
  , Op'Run
  , Op'Fly
  , Op'Switch
  ]

data BalloonState
  = NotReady
  | Running
  | Finished
  deriving (Eq, Show)

data Balloon
  = Balloon
  { _balloonLayer :: NamedWidget Op'Layer
  , _balloonText :: String
  , _eff :: Widget Eff'Display
  , __state :: BalloonState
  , _counter :: Int
  , _stayTime :: Int
  }

makeLenses ''Balloon

instance Conf "balloon" where
  type Required "balloon" =
    [ "windowTexture" >: SDL.Texture
    , "size" >: V2 Int
    ]

  type Optional "balloon" =
    [ "text" >: String
    , "stayTime" >: Int
    ]
  
  def = 
    #text @= ""
    <: #stayTime @= 10
    <: emptyRecord

wBalloon :: Given StyleSheet => WConfig "balloon" -> GameM (Widget Op'Balloon)
wBalloon (viewWConfig . giveWid #balloon -> ViewWConfig wix req opt) = go <$> new where
  new :: GameM Balloon
  new = Balloon
    <$> wLayer (conf @"layer" wix req (def @"layer"))
    <*> return (opt ^. #text)
    <*> return (effDisplay EaseOut 40 40)
    <*> return NotReady
    <*> return 0
    <*> return (opt ^. #stayTime)

  go :: Balloon -> Widget Op'Balloon
  go model = Widget $
    (\(Op'Reset t) -> continue $ go $ reset t model)
    @> (\(Op'Render _) -> lift $ render (given ^. wlocation wix) model)
    @> (\Op'Run -> continueM $ fmap go $ run model)
    @> (\Op'Fly -> continue $ go $ model & _state .~ Running & eff %~ (^.op'appear))
    @> (\Op'Switch -> (if model^._state == Finished && model^.eff^.op'isDisappeared then freeze' else continue) $ go model)
    @> emptyUnion

  reset :: String -> Balloon -> Balloon
  reset t model = model & counter .~ 0 & balloonText .~ t

  render :: V2 Int -> Balloon -> GameM ()
  render v model = do
    model^.balloonLayer^.op'renderAlpha (model^.eff^.op'getAlpha)
    when (model^.balloonText /= "") $
      renders white [ translate (v + V2 15 10) $ shaded black $ text (model^.balloonText) ]

  run :: Balloon -> GameM Balloon
  run model = case model^._state of
    Running | model^.counter >= model^.stayTime -> return $ model & _state .~ Finished & eff %~ (^. op'disappear)
    Running | model^.eff^.op'isAppeared -> return $ model & counter +~ 1
    _ -> model & eff %%~ (^.op'run)