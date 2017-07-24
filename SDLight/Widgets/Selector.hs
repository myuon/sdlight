module SDLight.Widgets.Selector
  ( wSelector
  , Op'Selector
  , op'renderBy
  , op'getSelecting
  , op'getPointer
  , op'getLabels
  , op'setLabels

  , wSelectLayer
  , Op'SelectLayer
  ) where

import qualified SDL as SDL
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Functor.Sum
import Linear.V2
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.Layer

data Selector
  = Selector
  { _labels :: [String]
  , _pointer :: Maybe Int
  , _selectNum :: Int
  , _selecting :: [Int]
  , _isFinished :: Bool
  }

makeLenses ''Selector

data Op'RenderBy br m r where
  Op'RenderBy :: (String -> Int -> Bool -> Bool -> GameM ()) -> Op'RenderBy Value GameM ()

data Op'GetSelecting br m r where
  Op'GetSelecting :: Op'GetSelecting Value Identity [Int]

data Op'GetPointer br m r where
  Op'GetPointer :: Op'GetPointer Value Identity (Maybe Int)

data Op'GetLabels br m r where
  Op'GetLabels :: Op'GetLabels Value Identity [String]

data Op'SetLabels br m r where
  Op'SetLabels :: [String] -> Op'SetLabels Self Identity a

op'renderBy :: Op'RenderBy ∈ xs => (String -> Int -> Bool -> Bool -> GameM ()) -> Getter (Widget xs) (GameM ())
op'renderBy = _value . Op'RenderBy

op'getSelecting :: Op'GetSelecting ∈ xs => Getter (Widget xs) [Int]
op'getSelecting = _value' Op'GetSelecting

op'getPointer :: Op'GetPointer ∈ xs => Getter (Widget xs) (Maybe Int)
op'getPointer = _value' Op'GetPointer

op'getLabels :: Op'GetLabels ∈ xs => Getter (Widget xs) [String]
op'getLabels = _value' Op'GetLabels

op'setLabels :: Op'SetLabels ∈ xs => [String] -> Getter (Widget xs) (Widget xs)
op'setLabels = _self' . Op'SetLabels

type Op'Selector =
  [ Op'Reset ()
  , Op'Render
  , Op'RenderBy
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

wSelector :: [String] -> Int -> Widget Op'Selector
wSelector = \labels selnum -> go $ new labels selnum where
  new :: [String] -> Int -> Selector
  new labels selectNum = Selector labels Nothing selectNum [] False

  go :: Selector -> Widget Op'Selector
  go sel = Widget $
    (\(Op'Reset _) -> continue $ go $ reset sel)
    @> (\(Op'Render _ v) -> lift $ renderDropdown sel v)
    @> (\(Op'RenderBy rend) -> lift $ render sel rend)
    @> (\Op'Run -> continueM $ fmap go $ return sel)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys sel)
    @> (\Op'Switch -> (if sel^.isFinished then freeze' else continue) $ go sel)
    @> (\Op'GetSelecting -> finish $ sel^.selecting)
    @> (\Op'GetPointer -> finish $ sel^.pointer)
    @> (\Op'GetLabels -> finish $ sel^.labels)
    @> (\(Op'SetLabels ls) -> continue $ go $ sel & labels .~ ls)
    @> emptyUnion

  reset :: Selector -> Selector
  reset sel = sel & pointer .~ Nothing & selecting .~ [] & isFinished .~ False

  render :: Selector -> (String -> Int -> Bool -> Bool -> GameM ()) -> GameM ()
  render sel rendItem = do
    forM_ (zip [0..] (sel^.labels)) $ \(i,label) ->
      rendItem label i (i `elem` (sel^.selecting)) (Just i == sel^.pointer)

  renderDropdown :: Selector -> V2 Int -> GameM ()
  renderDropdown sel p = do
    render sel $ \label i selecting focused -> do
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
        Just 0 -> return $ sel & pointer .~ Just (length (sel^.labels) - 1)
        Just p -> return $ sel & pointer .~ Just (p-1)
    | keys M.! SDL.ScancodeDown == 1 =
      case sel^.pointer of
        Nothing -> return $ sel & pointer .~ Just 0
        Just p | p == length (sel^.labels) - 1 -> return $ sel & pointer .~ Just 0
        Just p -> return $ sel & pointer .~ Just (p+1)
    | keys M.! SDL.ScancodeZ == 1 =
      case sel^.isFinished of
        False | isJust (sel^.pointer) -> do
          let p = fromJust $ sel^.pointer
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

wSelectLayer :: SDL.Texture -> SDL.Texture -> V2 Int -> [String] -> Int -> GameM (Widget Op'SelectLayer)
wSelectLayer = \win cur v labels num -> go <$> new win cur v labels num where
  new :: SDL.Texture -> SDL.Texture -> V2 Int -> [String] -> Int -> GameM SelectLayer
  new win cur v labels num = liftM3 (,,) (wLayer win v) (wLayer cur (V2 (v^._x - 20) 30)) (return $ wSelector labels num)
  
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
    (sel^._3^.) $ op'renderBy $ \label i selecting focused -> do
      when focused $ do
        sel^._2^.op'render (v + V2 10 (20+30*i))

      let color = if selecting then red else white
      renders color $
        [ translate (v + V2 (20+5) (20+30*i)) $ shaded black $ text label
        ]

