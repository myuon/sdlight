{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Selector where

import qualified SDL as SDL
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import SDLight.Types
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

newSelector :: [String] -> Int -> Selector
newSelector labels selectNum = Selector labels Nothing selectNum [] False

renderSelector :: Selector -> (String -> Int -> Bool -> Bool -> GameM ()) -> GameM ()
renderSelector sel rendItem = do
--  renderLayer (sel^.selectorDisplayLayer) (SDL.V2 200 300)
  
  forM_ (zip [0..] (sel^.labels)) $ \(i,label) ->
    rendItem label i (i `elem` (sel^.selecting)) (Just i == sel^.pointer)

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
      case sel^.isFinished of
        False | length (sel^.selecting) == sel^.selectNum -> return $ sel & isFinished .~ True
        False | isJust (sel^.pointer) ->
          let p = fromJust $ sel^.pointer in
          if p `elem` sel^.selecting then return $ sel & selecting %~ delete p
          else return $ sel & selecting %~ (p :)
        _ -> return sel
  | otherwise = return sel

