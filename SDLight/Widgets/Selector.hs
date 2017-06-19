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

data Selector
  = Selector
  { _labels :: [String]
  , _pointer :: Maybe Int
  , _selectNum :: Int
  , _selecting :: [Int]
  , _isFinished :: Bool
  }

makeLenses ''Selector

-- Layeredにする都合上RenderDropdownをRenderとして登録しておくけれど
-- あとで差し替えられるようにしよう

data Op'GetSelecting m r where
  Op'GetSelecting :: Op'GetSelecting Identity [Int]

data Op'GetPointer m r where
  Op'GetPointer :: Op'GetPointer Identity (Maybe Int)

data Op'SetLabels m r where
  Op'SetLabels :: [String] -> Op'SetLabels Identity NoValue

data Op'RenderBy m r where
  Op'RenderBy :: (String -> Int -> Bool -> Bool -> GameM ()) -> Op'RenderBy GameM ()

type Op'Selector =
  [ Op'Reset '[]
  , Op'Render
  , Op'RenderBy
  , Op'HandleEvent
  , Op'IsFinished
  , Op'GetSelecting
  , Op'GetPointer
  , Op'SetLabels
  ]

wSelector :: [String] -> Int -> Widget Op'Selector
wSelector = \labels selnum -> go $ new labels selnum where
  new :: [String] -> Int -> Selector
  new labels selectNum = Selector labels Nothing selectNum [] False

  go :: Selector -> Widget Op'Selector
  go sel = Widget $
    (\(Op'Reset _) -> continue go $ reset sel)
    @> (\(Op'Render v) -> lift $ renderDropdown sel v)
    @> (\(Op'RenderBy f) -> lift $ render sel f)
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

