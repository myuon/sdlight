{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Selector where

import qualified SDL as SDL
import qualified Data.Map as M
import Data.Maybe
import Control.Lens
import Control.Monad
import SDLight.Types

data SelectorState = Selecting | Selected

data Selector
  = Selector
  { _labels :: [String]
  , _pointer :: Maybe Int
  , _selectNum :: Int
  , _selecting :: [String]
  , _selectorState :: SelectorState
  }

makeLenses ''Selector

renderSelector :: Selector -> (String -> Int -> Bool -> Bool -> GameM ()) -> GameM ()
renderSelector sel rend = do
  forM_ (zip [0..] (sel^.labels)) $ \(i,label) ->
    rend label i (label `elem` (sel^.selecting)) (Just i == sel^.pointer)

handleSelectorEvent :: M.Map SDL.Scancode Int -> Selector -> GameM Selector
handleSelectorEvent keys sel
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
      case sel^.selectorState of
        Selecting | length (sel^.selecting) == sel^.selectNum -> return $ sel & selectorState .~ Selected
        Selecting | isJust (sel^.pointer) -> return $ sel & selecting %~ ((sel^.labels) !! (fromJust $ sel^.pointer) :)
        _ -> return sel
  | otherwise = return sel

