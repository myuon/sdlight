module SDLight.Widgets.Selector
  ( wSelector
  , Op'Selector
  , SelectorRenderConfig
  , op'renderSelector
  , op'getSelecting
  , op'getPointer
  , op'getLabels
  , op'setLabels

  , wSelectLayer
  , Op'SelectLayer

  , Op'GetSelecting(..)
  , Op'GetPointer(..)
  , Op'GetLabels(..)

  , SelectorConfig
  , SelectLayerConfig
  ) where

import qualified SDL as SDL
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Reflection
import Data.Default
import Data.Extensible
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans
import Linear.V2
import Data.Scoped
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Stylesheet
import SDLight.Widgets.Core
import SDLight.Widgets.Layer

data Selector
  = Selector
  { _labels :: [(Int,String)]
  , _pointer :: Maybe (Scoped Int)
  , _pagerStyle :: Maybe Int
  , _selectNum :: Int
  , _selecting :: [Int]
  , _isFinished :: Bool
  }

makeLenses ''Selector

type SelectorRenderConfig = Record
  [ "label" >: String
  , "index" >: Int
  , "isSelected" >: Bool
  , "isFocused" >: Bool
  , "location" >: V2 Int
  ]

makeOp "RenderSelector" [t| (SelectorRenderConfig -> GameM ()) -> _ Value GameM () |]
makeOp "GetSelecting" [t| _ Value Identity [Int] |]
makeOp "GetPointer" [t| _ Value Identity (Maybe Int) |]
makeOp "GetLabels" [t| _ Value Identity [String] |]
makeOp "SetLabels" [t| [String] -> Maybe Int -> _ Self Identity () |]

type Op'Selector =
  [ Op'Reset ()
  , Op'Render
  , Op'RenderSelector
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetSelecting
  , Op'GetPointer
  , Op'GetLabels
  , Op'SetLabels
  ]

-- とりあえずrenderDropDownの実装
-- 必要があればoverrideする

type SelectorConfig =
  [ "labels" >: [String]
  , "selectNum" >: Int
  , "pager" >: Maybe Int
  ]

instance Default (Config SelectorConfig) where
  def = Config
    $ #labels @= []
    <: #selectNum @= 1
    <: #pager @= Nothing
    <: emptyRecord
    
wSelector :: WConfig SelectorConfig -> Widget Op'Selector
wSelector (giveWid "selector" -> cfg) = go $ new where
  pointerFromPagerStyle labels pager = maybe (rangeScope labels (length labels - 1)) (rangeScope labels) pager

  new :: Selector
  new = Selector
    (zip [0..] $ cfg ^. _Wrapped . #labels)
    (pointerFromPagerStyle (cfg ^. _Wrapped . #labels) (cfg ^. _Wrapped . #pager))
    (cfg ^. _Wrapped . #pager)
    (cfg ^. _Wrapped . #selectNum)
    []
    False

  go :: Selector -> Widget Op'Selector
  go sel = Widget $
    (\(Op'Reset _) -> continue $ go $ reset sel)
    @> (\(Op'Render _) -> lift $ renderDropdown sel (getLocation cfg))
    @> (\(Op'RenderSelector rend) -> lift $ render sel rend)
    @> (\Op'Run -> continueM $ fmap go $ return sel)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys sel)
    @> (\Op'Switch -> (if sel^.isFinished then freeze' else continue) $ go sel)
    @> (\Op'GetSelecting -> finish $ sel^.selecting)
    @> (\Op'GetPointer -> finish $ sel^.pointer <&> (^.scoped))
    @> (\Op'GetLabels -> finish $ fmap snd $ sel^.labels)
    @> (\(Op'SetLabels ls pager) -> continue $ go $ sel & labels .~ zip [0..] ls & pointer .~ pointerFromPagerStyle ls pager & pagerStyle .~ pager)
    @> emptyUnion

  reset :: Selector -> Selector
  reset sel = sel & pointer._Just %~ adjustTo0 & selecting .~ [] & isFinished .~ False

  render :: Selector -> (SelectorRenderConfig -> GameM ()) -> GameM ()
  render sel rendItem = do
    forM_ (zip [0..] $ fmap ((sel^.labels) !!) $ maybe [0..length (sel^.labels)-1] rangeOf (sel^.pointer)) $ \(i,label) ->
      rendItem
        $ #label @= (snd label)
        <: #index @= i
        <: #isSelected @= (i `elem` (sel^.selecting))
        <: #isFocused @= (Just (fst label) == ((sel^.pointer) <&> (^.scoped)))
        <: #location @= getLocation cfg
        <: emptyRecord

  renderDropdown :: Selector -> V2 Int -> GameM ()
  renderDropdown sel p = do
    render sel $ \rcfg -> do
      when (rcfg ^. #isFocused) $ do
        renders white $
          [ translate (p + V2 20 (20 + 30 * (rcfg ^. #index))) $ shaded black $ text "▶"
          ]

      let color = if rcfg ^. #isSelected then red else white
      renders color $
        [ translate (p + V2 (20+20) (20 + 30 * (rcfg ^. #index))) $ shaded black $ text $ rcfg ^. #label
        ]

  handler :: M.Map SDL.Scancode Int -> Selector -> GameM Selector
  handler keys sel
    | keyjudge (keys M.! SDL.ScancodeUp) = return $ sel & pointer._Just %~ back
    | keyjudge (keys M.! SDL.ScancodeDown) = return $ sel & pointer._Just %~ forward
    | keys M.! SDL.ScancodeZ == 1 && not (sel^.isFinished) && (isJust $ sel^.pointer) = do
        let p = fst $ (sel^.labels) !! (sel^.pointer^?!_Just^.scoped)
        if p `elem` sel^.selecting
          then return $ sel & selecting %~ delete p
          else return $ sel
               & selecting %~ (p :)
               & isFinished .~ (length (sel^.selecting) + 1 == sel^.selectNum)
    | otherwise = return sel
    where
      keyjudge n | n < 100 = n `mod` 20 == 1
      keyjudge n = n `mod` 7 == 1

type Op'SelectLayer =
  [ Op'Reset ()
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetSelecting
  , Op'GetPointer
  , Op'GetLabels
  , Op'SetLabels
  ]

type SelectLayer = (NamedWidget Op'Layer, NamedWidget Op'Layer, Widget Op'Selector)

type SelectLayerConfig =
  [ "windowTexture" >: SDL.Texture
  , "cursorTexture" >: SDL.Texture
  , "size" >: V2 Int
  , "selectorConfig" >: Record SelectorConfig
  ]

instance Default (Config SelectLayerConfig) where
  def = Config
    $ #windowTexture @= error "not initialized"
    <: #cursorTexture @= error "not initialized"
    <: #size @= V2 100 200
    <: #selectorConfig @= getConfig def
    <: emptyRecord
    
wSelectLayer :: Given StyleSheet => WConfig SelectLayerConfig -> GameM (Widget Op'SelectLayer)
wSelectLayer (giveWid "select-layer" -> cfg) = go <$> new where
  new :: GameM SelectLayer
  new = liftM3 (,,)
    (wLayer (cfgs _Wrapped $ shrinkAssoc @_ @LayerConfig (cfg ^. _Wrapped)))
    (wLayer (cfgs _Wrapped $ #wix @= (cfg ^. _Wrapped . #wix) <: #windowTexture @= (cfg ^. _Wrapped . #cursorTexture) <: #size @= V2 (cfg ^. _Wrapped . #size ^. _x - 20) 30 <: emptyRecord))
    (return $ wSelector $ cfgstyle cfg $ cfg ^. _Wrapped . #selectorConfig)

  go :: SelectLayer -> Widget Op'SelectLayer
  go w = Widget $
    (\(Op'Reset args) -> continue $ go $ w & _3 ^%~ op'reset args)
    @> (\(Op'Render _) -> lift $ render (getLocation cfg) w)
    @> (\Op'Run -> continue $ go w)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ (\x -> w & _3 .~ x) <$> (w^._3^.op'handleEvent keys))
    @> (\Op'Switch -> (if op'isFreeze (w^._3) op'switch then freeze' else continue) $ go w)
    @> (\Op'GetSelecting -> finish $ w^._3^.op'getSelecting)
    @> (\Op'GetPointer -> finish $ w^._3^.op'getPointer)
    @> (\Op'GetLabels -> finish $ w^._3^.op'getLabels)
    @> (\(Op'SetLabels t pager) -> continue $ go $ w & _3 ^%~ op'setLabels t pager)
    @> emptyUnion

  render :: V2 Int -> SelectLayer -> GameM ()
  render v sel = do
    sel^._1^.op'renderAt v 1.0
    (sel^._3^.) $ op'renderSelector $ \rcfg -> do
      when (rcfg ^. #isFocused) $ do
        sel^._2^.op'renderAt (v + (rcfg ^. #location) + V2 0 (30 * (rcfg ^. #index))) 1.0

      let color = if rcfg ^. #isSelected then red else white
      renders color $
        [ translate (v + (rcfg ^. #location) + V2 10 (30 * (rcfg ^. #index))) $ shaded black $ text $ rcfg ^. #label
        ]

