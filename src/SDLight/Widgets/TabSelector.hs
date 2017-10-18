module SDLight.Widgets.TabSelector
  ( op'getTabName
  , op'renderTabSelector
  , op'getCurrentSelector
  , op'setTabs
  , Op'TabSelector
  , wTabSelector
  , Op'TabSelectLayer
  , wTabSelectLayer

  , TabSelectorRenderConfig
  ) where

import qualified SDL as SDL
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Reflection
import Data.Extensible
import qualified Data.Map as M
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Stylesheet
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Selector

data TabSelector
  = TabSelector
  { _wtabs :: [(String, NamedWidget Op'Selector)]
  , _pointer :: Maybe Int
  }

makeLenses ''TabSelector

type TabSelectorRenderConfig = Record
  [ "tabName" >: String
  , "tabIndex" >: Int
  , "isTabSelected" >: Bool
  ]

makeOp "GetTabName" [t| _ Value Identity (Maybe String) |]
makeOp "RenderTabSelector" [t| (TabSelectorRenderConfig -> SelectorRenderConfig -> GameM ()) -> _ Value GameM () |]
makeOp "GetCurrentSelector" [t| _ Value Identity (Maybe (NamedWidget Op'Selector)) |]
makeOp "SetTabs" [t| [(String, [String])] -> _ Self Identity () |]

type Op'TabSelector =
  [ Op'Reset ()
  , Op'Render
  , Op'RenderTabSelector
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetSelecting
  , Op'GetPointer
  , Op'GetCurrentSelector
  , Op'GetTabName
  , Op'SetTabs
  ]

instance Conf "tab_selector" where
  type Required "tab_selector" = '[]
  type Optional "tab_selector" =
    [ "selectNum" >: Int
    , "pager" >: Maybe Int
    ]
  
  def =
    #selectNum @= 1
    <: #pager @= Nothing
    <: emptyRecord

wTabSelector :: Given StyleSheet => WConfig "tab_selector" -> Widget Op'TabSelector
wTabSelector (wconf #tab_selector -> ViewWConfig wix req opt) = go new where
  new = TabSelector [] Nothing

  go :: TabSelector -> Widget Op'TabSelector
  go model = Widget $
    (\(Op'Reset _) -> continue $ go $ reset model)
    @> (\(Op'Render _) -> lift $ renderDropdown (getLocation wix) model)
    @> (\(Op'RenderTabSelector rend) -> lift $ render rend model)
    @> (\Op'Run -> continueM $ fmap go $ model & wtabs.each._2 %%~ (^.op'run))
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys model)
    @> (\Op'Switch -> (if any (\p -> (p^._2) `op'isFreeze` op'switch) (model^.wtabs) then freeze' else continue) $ go model)
    @> (\Op'GetSelecting -> finish $ maybe [] (^.op'getSelecting) (go model^.op'getCurrentSelector))
    @> (\Op'GetPointer -> finish $ go model^.op'getCurrentSelector^?!_Just^.op'getPointer)
    @> (\Op'GetCurrentSelector -> finish $ maybe Nothing (\p -> model^.wtabs^?ix p._2) (model^.pointer))
    @> (\Op'GetTabName -> finish $ maybe Nothing (\p -> model^.wtabs^?ix p._1) $ model^.pointer)
    @> (\(Op'SetTabs ts) ->
      let wcfg s = conf @"selector" wix emptyRecord (#labels @= s <: opt) in
      continue $ go $ model & wtabs .~ fmap (second (wSelector . wcfg)) ts & pointer .~ (if ts /= [] then Just 0 else Nothing))
    @> emptyUnion

  reset model = model &~ do
    wtabs.each._2 %= (^. op'reset ())
    pointer .= Nothing

  renderDropdown :: V2 Int -> TabSelector -> GameM ()
  renderDropdown v model = case model^.pointer of
    Nothing -> return ()
    Just here -> do
      forM_ (zip [0..] $ model^.wtabs) $ \(i,(tab,_)) ->
        renders white [ translate (V2 (i*50) 0 + v) $ shaded black $ text tab ]
      renders white [ translate (V2 (here*50) 0 + v) $ shaded black $ text "▶" ]

      model^.wtabs^?!ix here^._2.op'render

  render :: (TabSelectorRenderConfig -> SelectorRenderConfig -> GameM ()) -> TabSelector -> GameM ()
  render rend model = do
    forM_ (zip [0..] $ model^.wtabs) $ \(i,(name,wtab)) -> do
      wtab^.op'renderSelector (rend
        $ #tabName @= name
        <: #tabIndex @= i
        <: #isTabSelected @= (model^.pointer == Just i)
        <: emptyRecord)

  handler keys model = case model^.pointer of
    Nothing -> return model
    Just tab | keys M.! SDL.ScancodeRight == 1 ->
      return $ model & pointer .~ if tab == length (model^.wtabs) - 1 then Just 0 else Just (tab + 1)
    Just tab | keys M.! SDL.ScancodeLeft == 1 ->
      return $ model & pointer .~ if tab == 0 then Just (length (model^.wtabs) - 1) else Just (tab - 1)
    Just tab -> fmap (\w -> model & wtabs.ix tab._2 .~ w) $ model^.wtabs^?!ix tab^._2^.op'handleEvent keys

type Op'TabSelectLayer =
  [ Op'Reset ()
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetSelecting
  , Op'GetPointer
  , Op'GetCurrentSelector
  , Op'GetTabName
  , Op'SetTabs
  ]

type TabSelectLayer = (NamedWidget Op'Layer, Layer, Widget Op'TabSelector)

instance Conf "tab_select_layer" where
  type Required "tab_select_layer" =
    [ "windowTexture" >: SDL.Texture
    , "cursorTexture" >: SDL.Texture
    , "size" >: V2 Int
    ]
  type Optional "tab_select_layer" =
    [ "tabWidth" >: Int
    , "selectNum" >: Int
    , "pager" >: Maybe Int
    ]
  
  def =
    #tabWidth @= 100
    <: def @"tab_selector"

wTabSelectLayer :: Given StyleSheet => WConfig "tab_select_layer" -> GameM (Widget Op'TabSelectLayer)
wTabSelectLayer (wconf #tab_select_layer -> ViewWConfig wix req opt) = go <$> new where
  new :: GameM TabSelectLayer
  new =
    liftM3 (,,)
    (wLayer $ conf @"layer" wix (shrinkAssoc req) (def @"layer"))
    (newLayer (req ^. #cursorTexture) (V2 (opt ^. #tabWidth) 30))
    (return $ wTabSelector $ conf @"tab_selector" wix emptyRecord (shrinkAssoc opt))

  go :: TabSelectLayer -> Widget Op'TabSelectLayer
  go model = Widget $
    (\(Op'Reset args) -> continue $ go $ model & _3 ^%~ op'reset args)
    @> (\(Op'Render _) -> lift $ render (getLocation wix) model)
    @> (\Op'Run -> continue $ go model)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ model & _3 ^%%~ op'handleEvent keys)
    @> (\Op'Switch -> (if op'isFreeze (model^._3) op'switch then freeze' else continue) $ go model)
    @> (\Op'GetSelecting -> finish $ model^._3^.op'getSelecting)
    @> (\Op'GetPointer -> finish $ model^._3^.op'getPointer)
    @> (\Op'GetCurrentSelector -> finish $ model^._3^.op'getCurrentSelector)
    @> (\Op'GetTabName -> finish $ model^._3^.op'getTabName)
    @> (\(Op'SetTabs ts) -> continue $ go $ model & _3 ^%~ op'setTabs ts)
    @> emptyUnion

  render :: V2 Int -> TabSelectLayer -> GameM ()
  render v model = do
    model^._1^.op'render
    (model^._3^.) $ op'renderTabSelector $ \tcfg scfg -> do
      when (tcfg ^. #isTabSelected) $ do
        model^._2^.op'renderLayer (v + V2 ((opt ^. #tabWidth) * (tcfg ^. #tabIndex)) 0) 1.0
      
      let color = if tcfg ^. #isTabSelected then red else white
      renders color $
        [ translate (v + V2 ((opt ^. #tabWidth) * (tcfg ^. #tabIndex)) 0) $ shaded black $ text $ tcfg ^. #tabName
        ]

      when (tcfg ^. #isTabSelected && scfg ^. #isFocused) $ do
        model^._2^.op'renderLayer (v + V2 10 (30 + 20 + 30 * (scfg ^. #index))) 1.0

      when (tcfg ^. #isTabSelected) $ do
        let color = if scfg ^. #isSelected then red else white
        renders color $
          [ translate (v + V2 (20+5) (30 + 20 + 30 * (scfg ^. #index))) $ shaded black $ text $ scfg ^. #label
          ]

