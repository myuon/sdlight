{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Selector
  ( wSelector
  , Op'Selector
  , Op'RenderBy(..)
  , Op'GetSelecting(..)
  , Op'GetPointer(..)
  , Op'SetLabels(..)

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

data Op'RenderBy m r where
  Op'RenderBy :: (String -> Int -> Bool -> Bool -> GameM ()) -> Op'RenderBy GameM ()

data Op'GetSelecting m r where
  Op'GetSelecting :: Op'GetSelecting Identity [Int]

data Op'GetPointer m r where
  Op'GetPointer :: Op'GetPointer Identity (Maybe Int)

data Op'SetLabels m r where
  Op'SetLabels :: [String] -> Op'SetLabels Identity NoValue

type Op'Selector =
  [ Op'Reset '[]
  , Op'Render
  , Op'RenderBy
  , Op'Run
  , Op'HandleEvent
  , Op'IsFinished
  , Op'GetSelecting
  , Op'GetPointer
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
    (\(Op'Reset _) -> continue go $ reset sel)
    @> (\(Op'Render v) -> lift $ renderDropdown sel v)
    @> (\(Op'RenderBy rend) -> lift $ render sel rend)
    @> (\Op'Run -> continueM go $ return sel)
    @> (\(Op'HandleEvent keys) -> continueM go $ handler keys sel)
    @> (\Op'IsFinished -> finish $ sel^.isFinished)
    @> (\Op'GetSelecting -> finish $ sel^.selecting)
    @> (\Op'GetPointer -> finish $ sel^.pointer)
    @> (\(Op'SetLabels ls) -> continue go $ sel & labels .~ ls)
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
  [ Op'Reset '[]
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'IsFinished
  , Op'GetSelecting
  , Op'GetPointer
  , Op'SetLabels
  ]

type SelectLayer = (Widget Op'Layer, Widget Op'Layer, Widget Op'Selector)

wSelectLayer :: FilePath -> FilePath -> V2 Int -> [String] -> Int -> GameM (Widget Op'SelectLayer)
wSelectLayer = \win cur v labels num -> go <$> new win cur v labels num where
  new :: FilePath -> FilePath -> V2 Int -> [String] -> Int -> GameM SelectLayer
  new win cur v labels num = liftM3 (,,) (wLayer win v) (wLayer cur (V2 (v^._x - 20) 30)) (return $ wSelector labels num)
  
  go :: SelectLayer -> Widget Op'SelectLayer
  go w = Widget $
    (\(Op'Reset args) -> continue go $ w & _3 @%~ Op'Reset args)
    @> (\(Op'Render v) -> lift $ render w v)
    @> (\Op'Run -> continueM go $ return w)
    @> (\(Op'HandleEvent keys) -> continueM go $ (\x -> w & _3 .~ x) <$> (w^._3 @. Op'HandleEvent keys))
    @> (\Op'IsFinished -> finish $ w^._3 @@! Op'IsFinished)
    @> (\Op'GetSelecting -> finish $ w^._3 @@! Op'GetSelecting)
    @> (\Op'GetPointer -> finish $ w^._3 @@! Op'GetPointer)
    @> (\(Op'SetLabels t) -> continue go $ w & _3 @%~ Op'SetLabels t)
    @> emptyUnion
  
  render :: SelectLayer -> V2 Int -> GameM ()
  render sel v = do
    sel^._1 @! Op'Render v
    (sel^._3 @!) $ Op'RenderBy $ \label i selecting focused -> do
      when focused $ do
        sel^._2 @! Op'Render (v + V2 10 (20+30*i))

      let color = if selecting then red else white
      renders color $
        [ translate (v + V2 (20+5) (20+30*i)) $ shaded black $ text label
        ]

