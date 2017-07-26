module SDLight.Widgets.TabSelector
  (
  ) where

import qualified SDL as SDL
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Selector

data TabSelector
  = TabSelector
  { _wtabs :: [(String, Widget Op'Selector)]
  , _pointer :: Maybe Int
  }

makeLenses ''TabSelector

data TabSelectorConfig
  = TabSelectorConfig
  { _CfgTabName :: String
  , _CfgTabIndex :: Int
  , _CfgIsTabSelected :: Bool
  }

data Op'GetTabName br m a where
  Op'GetTabName :: Op'GetTabName Value Identity (Maybe String)

data Op'RenderTabSelector br m r where
  Op'RenderTabSelector :: (TabSelectorConfig -> SelectorConfig -> GameM ()) -> Op'RenderTabSelector Value GameM ()

data Op'GetCurrentSelector br m r where
  Op'GetCurrentSelector :: Op'GetCurrentSelector Value Identity (Maybe (Widget Op'Selector))

data Op'SetTabs br m r where
  Op'SetTabs :: [(String, [String])] -> Op'SetTabs Self Identity a

op'getTabName :: Op'GetTabName ∈ xs => Getter (Widget xs) (Maybe String)
op'getTabName = to $ \w -> w ^. _value' Op'GetTabName

op'getCurrentSelector :: Op'GetCurrentSelector ∈ xs => Getter (Widget xs) (Maybe (Widget Op'Selector))
op'getCurrentSelector = to $ \w -> w ^. _value' Op'GetCurrentSelector

op'renderTabSelector :: Op'RenderTabSelector ∈ xs => (TabSelectorConfig -> SelectorConfig -> GameM ()) -> Getter (Widget xs) (GameM ())
op'renderTabSelector = _value . Op'RenderTabSelector

op'setTabs :: Op'SetTabs ∈ xs => [(String, [String])] -> Getter (Widget xs) (Widget xs)
op'setTabs t = to $ \w -> w ^. _self' (Op'SetTabs t)

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

wTabSelector :: Int -> Widget Op'TabSelector
wTabSelector selnum = go new where
  new = TabSelector [] Nothing

  go :: TabSelector -> Widget Op'TabSelector
  go model = Widget $
    (\(Op'Reset _) -> continue $ go $ reset model)
    @> (\(Op'Render _ v) -> lift $ renderDropdown v model)
    @> (\(Op'RenderTabSelector rend) -> lift $ render rend model)
    @> (\Op'Run -> continueM $ fmap go $ model & wtabs.each._2 %%~ (^.op'run))
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys model)
    @> (\Op'Switch -> (if any (\p -> (p^._2) `op'isFreeze` op'switch) (model^.wtabs) then freeze' else continue) $ go model)
    @> (\Op'GetSelecting -> finish $ maybe [] (^.op'getSelecting) (go model^.op'getCurrentSelector))
    @> (\Op'GetPointer -> finish $ maybe Nothing (^.op'getPointer) (go model^.op'getCurrentSelector))
    @> (\Op'GetCurrentSelector -> finish $ maybe Nothing (\p -> model^.wtabs^?ix p._2) (model^.pointer))
    @> (\Op'GetTabName -> finish $ maybe Nothing (\p -> model^.wtabs^?ix p._1) $ model^.pointer)
    @> (\(Op'SetTabs ts) -> continue $ go $ model & wtabs .~ fmap (second (\s -> wSelector s selnum)) ts)
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

      model^.wtabs^?!ix here^._2.op'render (V2 0 40 + v)

  render :: (TabSelectorConfig -> SelectorConfig -> GameM ()) -> TabSelector -> GameM ()
  render rend model = do
    forM_ (zip [0..] $ model^.wtabs) $ \(i,(name,wtab)) -> do
      wtab^.op'renderSelector (rend (TabSelectorConfig name i (model^.pointer == Just i)))

  handler keys model = case model^.pointer of
    Nothing -> return model
    Just tab | keys M.! SDL.ScancodeRight == 1 ->
      return $ model & pointer .~ if tab == length (model^.wtabs) - 1 then Just 0 else Just (tab + 1)
    Just tab | keys M.! SDL.ScancodeLeft == 1 ->
      return $ model & pointer .~ if tab == 0 then Just (length (model^.wtabs) - 1) else Just (tab - 1)


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

type TabSelectLayer = (Widget Op'Layer, Widget Op'Layer, Widget Op'TabSelector)

wTabSelectLayer :: SDL.Texture -> SDL.Texture -> V2 Int -> Int -> GameM (Widget Op'TabSelectLayer)
wTabSelectLayer = \win cur v num -> go <$> new win cur v num where
  new win cur v num =
    liftM3 (,,)
    (wLayer win v)
    (wLayer cur (V2 (v^._x - 20) 30))
    (return $ wTabSelector num)

  go :: TabSelectLayer -> Widget Op'TabSelectLayer
  go model = Widget $
    (\(Op'Reset args) -> continue $ go $ model & _3 ^%~ op'reset args)
    @> (\(Op'Render _ v) -> lift $ render v model)
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
    model^._1^.op'render v
    (model^._3^.) $ op'renderTabSelector $ \tcfg scfg -> do
      when (_CfgIsTabSelected tcfg) $ do
        model^._2^.op'render (v + V2 (30*_CfgTabIndex tcfg) 0)
      
      when (_CfgIsFocused scfg) $ do
        model^._2^.op'render (v + V2 10 (30+20+30*_CfgIndex scfg))

      let color = if _CfgIsSelected scfg then red else white
      renders color $
        [ translate (v + V2 (20+5) (20+30*_CfgIndex scfg)) $ shaded black $ text $ _CfgText scfg
        ]

