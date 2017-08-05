module SDLight.Widgets.Selector
  ( wSelector
  , Op'Selector
  , SelectorRenderConfig(..)
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

data SelectorRenderConfig
  = SelectorRenderConfig
  { _CfgText :: String
  , _CfgIndex :: Int
  , _CfgIsSelected :: Bool
  , _CfgIsFocused :: Bool
  }

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
wSelector = \cfg -> go $ new cfg where
  pointerFromPagerStyle labels pager = maybe (rangeScope labels (length labels - 1)) (rangeScope labels) pager

  new :: WConfig SelectorConfig -> Selector
  new (Config cfg) = Selector
    (zip [0..] $ cfg ^. #labels)
    (pointerFromPagerStyle (cfg ^. #labels) (cfg ^. #pager))
    (cfg ^. #pager)
    (cfg ^. #selectNum)
    []
    False

  go :: Selector -> Widget Op'Selector
  go sel = Widget $
    (\(Op'Reset _) -> continue $ go $ reset sel)
    @> (\(Op'Render _ v) -> lift $ renderDropdown sel v)
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
      rendItem $ SelectorRenderConfig (snd label) i (i `elem` (sel^.selecting)) (Just (fst label) == ((sel^.pointer) <&> (^.scoped)))

  renderDropdown :: Selector -> V2 Int -> GameM ()
  renderDropdown sel p = do
    render sel $ \(SelectorRenderConfig label i selecting focused) -> do
      when focused $ do
        renders white $
          [ translate (p + V2 20 (20+30*i)) $ shaded black $ text "▶"
          ]

      let color = if selecting then red else white
      renders color $
        [ translate (p + V2 (20+20) (20+30*i)) $ shaded black $ text label
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
wSelectLayer = \cfg -> go <$> new (cfg & _Wrapped . #wix %~ (</> WId "select-layer")) where
  new :: WConfig SelectLayerConfig -> GameM SelectLayer
  new (Config cfg) = liftM3 (,,)
    (wLayer (cfgs _Wrapped $ shrinkAssoc @_ @LayerConfig cfg))
    (wLayer (cfgs _Wrapped $ #wix @= (cfg ^. #wix) <: #windowTexture @= (cfg ^. #cursorTexture) <: #size @= V2 (cfg ^. #size ^. _x - 20) 30 <: emptyRecord))
    (return $ wSelector $ cfgs _Wrapped $ #wix @= (cfg ^. #wix) <: cfg ^. #selectorConfig)
  
  go :: SelectLayer -> Widget Op'SelectLayer
  go w = Widget $
    (\(Op'Reset args) -> continue $ go $ w & _3 ^%~ op'reset args)
    @> (\(Op'Render _ v) -> lift $ render w v)
    @> (\Op'Run -> continue $ go w)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ (\x -> w & _3 .~ x) <$> (w^._3^.op'handleEvent keys))
    @> (\Op'Switch -> (if op'isFreeze (w^._3) op'switch then freeze' else continue) $ go w)
    @> (\Op'GetSelecting -> finish $ w^._3^.op'getSelecting)
    @> (\Op'GetPointer -> finish $ w^._3^.op'getPointer)
    @> (\Op'GetLabels -> finish $ w^._3^.op'getLabels)
    @> (\(Op'SetLabels t pager) -> continue $ go $ w & _3 ^%~ op'setLabels t pager)
    @> emptyUnion

  render :: SelectLayer -> V2 Int -> GameM ()
  render sel v = do
    sel^._1^.op'render v
    (sel^._3^.) $ op'renderSelector $ \cfg -> do
      when (_CfgIsFocused cfg) $ do
        sel^._2^.op'render (v + V2 10 (20+30*_CfgIndex cfg))

      let color = if _CfgIsSelected cfg then red else white
      renders color $
        [ translate (v + V2 (20+5) (20+30*_CfgIndex cfg)) $ shaded black $ text $ _CfgText cfg
        ]

