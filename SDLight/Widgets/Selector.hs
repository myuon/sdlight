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
  ) where

import qualified SDL as SDL
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.TH
import SDLight.Widgets.Layer

data Selector
  = Selector
  { _labels :: [(Int,String)]
  , _pointer :: Maybe Int
  , _pager :: Int
  , _pageLineNum :: Maybe Int
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
makeOp "SetLabels" [t| [String] -> _ Self Identity () |]

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

wSelector :: [String] -> Int -> Maybe Int -> Widget Op'Selector
wSelector = \labels selnum page -> go $ new labels selnum page where
  new labels selectNum page = Selector (zip [0..] labels) Nothing 0 page selectNum [] False

  go :: Selector -> Widget Op'Selector
  go sel = Widget $
    (\(Op'Reset _) -> continue $ go $ reset sel)
    @> (\(Op'Render _ v) -> lift $ renderDropdown sel v)
    @> (\(Op'RenderSelector rend) -> lift $ render sel rend)
    @> (\Op'Run -> continueM $ fmap go $ return sel)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys sel)
    @> (\Op'Switch -> (if sel^.isFinished then freeze' else continue) $ go sel)
    @> (\Op'GetSelecting -> finish $ sel^.selecting)
    @> (\Op'GetPointer -> finish $ sel^.pointer)
    @> (\Op'GetLabels -> finish $ fmap snd $ sel^.labels)
    @> (\(Op'SetLabels ls) -> continue $ go $ sel & labels .~ zip [0..] ls)
    @> emptyUnion

  reset :: Selector -> Selector
  reset sel = sel & pointer .~ Nothing & selecting .~ [] & isFinished .~ False

  render :: Selector -> (SelectorRenderConfig -> GameM ()) -> GameM ()
  render sel rendItem = do
    let itemNum = maybe (length $ sel^.labels) id $ sel^.pageLineNum
    forM_ (zip [0..] $ drop (sel^.pager) $ take (itemNum + sel^.pager) (sel^.labels)) $ \(i,label) ->
      rendItem $ SelectorRenderConfig (snd label) i (i `elem` (sel^.selecting)) (Just i == sel^.pointer)

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
    | keys M.! SDL.ScancodeUp == 1 =
      case sel^.pointer of
        Nothing -> return $ sel & pointer .~ Just 0
        Just 0 ->
          if sel^.pager == 0
          then return $ sel
               & pointer .~ Just (maybe (length $ sel^.labels) id (sel^.pageLineNum) `min` (length $ sel^.labels) - 1)
               & pager .~ (length (sel^.labels) - maybe (length $ (sel^.labels)) id (sel^.pageLineNum)) `max` 0
          else return $ sel & pager -~ 1
        Just p -> return $ sel & pointer .~ Just (p-1)
    | keys M.! SDL.ScancodeDown == 1 =
      case sel^.pointer of
        Nothing -> return $ sel & pointer .~ Just 0
        Just p | p >= (maybe (length (sel^.labels)) id (sel^.pageLineNum) `min` (length $ sel^.labels)) - 1 ->
          if sel^.pager >= length (sel^.labels) - maybe (length $ (sel^.labels)) id (sel^.pageLineNum)
          then return $ sel & pointer .~ Just 0 & pager .~ 0
          else return $ sel & pager +~ 1
        Just p -> return $ sel & pointer .~ Just (p+1)
    | keys M.! SDL.ScancodeZ == 1 =
      case sel^.isFinished of
        False | isJust (sel^.pointer) -> do
          let p = fst $ ((sel^.labels) !!) $ fromJust $ sel^.pointer
          if p `elem` sel^.selecting
            then return $ sel & selecting %~ delete p
            else return $ sel
                   & selecting %~ (p :)
                   & isFinished .~ (length (sel^.selecting) + 1 == sel^.selectNum)
        _ -> return sel
    | otherwise = return sel


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

type SelectLayer = (Widget Op'Layer, Widget Op'Layer, Widget Op'Selector)

wSelectLayer :: SDL.Texture -> SDL.Texture -> V2 Int -> [String] -> Int -> Maybe Int -> GameM (Widget Op'SelectLayer)
wSelectLayer = \win cur v labels num page -> go <$> new win cur v labels num page where
  new win cur v labels num page = liftM3 (,,) (wLayer win v) (wLayer cur (V2 (v^._x - 20) 30)) (return $ wSelector labels num page)
  
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
    @> (\(Op'SetLabels t) -> continue $ go $ w & _3 ^%~ op'setLabels t)
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

