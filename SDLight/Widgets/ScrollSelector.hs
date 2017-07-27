module SDLight.Widgets.ScrollSelector
  ( Op'ScrollSelector
  , wScrollSelector
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad.Trans
import qualified Data.Map as M
import SDLight.Types
import SDLight.Widgets.Core
import SDLight.Widgets.Selector

data ScrollSelector
  = ScrollSelector
  { _numberOflines :: Int
  , _labels :: [String]
  , _pointer :: Maybe Int
  , _pageIndex :: Int
  , _selector :: Widget Op'Selector
  }

makeLenses ''ScrollSelector

type Op'ScrollSelector =
  [ Op'Reset ()
  , Op'Render
  , Op'Run
  , Op'Switch
  , Op'HandleEvent
  ]

wScrollSelector :: Int -> [String] -> Int -> Widget Op'ScrollSelector
wScrollSelector = \num labels sel -> go $ new num labels sel where
  new num labels sel = ScrollSelector num labels Nothing 0 (wSelector (take num labels) sel)

  go :: ScrollSelector -> Widget Op'ScrollSelector
  go model = Widget $
    (\(Op'Reset _) -> continue $ go $ model & selector ^%~ op'reset ())
    @> (\(Op'Render _ v) -> lift $ model^.selector^.op'render v)
    @> (\Op'Run -> continue $ go model)
    @> (\Op'Switch -> bimapT (\s -> go $ model & selector .~ s) id $ model^.selector^.op'switch)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys model)
    @> emptyUnion

  handler :: M.Map SDL.Scancode Int -> ScrollSelector -> GameM ScrollSelector
  handler keys model = case model^.pointer of
    Nothing | any (\k -> keys M.! k == 1) [SDL.ScancodeUp, SDL.ScancodeDown] -> return $ model & pointer .~ Just 0
    Just p | model^.pageIndex == 0 && keys M.! SDL.ScancodeUp == 1 ->
      return $ model
        & pointer .~ (if p == 0 then Just (model^.numberOflines - 1) else Just (p-1))
        & pageIndex %~ (if p == 0 then const (length (model^.labels) - model^.numberOflines) else id)
    Just p | keys M.! SDL.ScancodeUp == 1 ->
      return $ model
        & pointer .~ (if p == 0 then Just 0 else Just (p-1))
        & pageIndex %~ (if p == 0 then subtract 1 else id)
    Just p | model^.pageIndex == length (model^.labels) - model^.numberOflines && keys M.! SDL.ScancodeDown == 1 ->
      return $ model
        & pointer .~ (if p == model^.numberOflines - 1 then Just 0 else Just (p-1))
        & pageIndex %~ (if p == model^.numberOflines - 1 then const 0 else id)
    Just p | keys M.! SDL.ScancodeDown == 1 ->
      return $ model
        & pointer .~ (if p == model^.numberOflines - 1 then Just 0 else Just (p-1))
        & pageIndex %~ (if p == model^.numberOflines - 1 then id else (+1))

