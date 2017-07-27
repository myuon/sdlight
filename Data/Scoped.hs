module Data.Scoped where

import Control.Lens
import Data.List
import Data.Ix

data Scoped a
  = Scoped
  { _scoped :: a
  , _locally :: (a,a)
  , _globally :: (a,a)
  }

makeLenses ''Scoped

rangeScope :: [a] -> Int -> Maybe (Scoped Int)
rangeScope [] _ = Nothing
rangeScope xs n = Just $ scopedTo n (length xs - 1)

rangeOf :: Ix a => Scoped a -> [a]
rangeOf sc = range (sc^.locally) `intersect` range (sc^.globally)

scopedTo :: (Num a, Ord a, Show a) => a -> a -> Scoped a
scopedTo local global | 0 <= local && 0 <= global = Scoped 0 (0,local) (0,global)
scopedTo local global = error $ show (local,global)

adjustTo0 :: Num a => Scoped a -> Scoped a
adjustTo0 sc = sc & scoped .~ 0 & locally %~ (\(a,b) -> (0,b-a)) & globally %~ (\(a,b) -> (0,b-a))

localIx :: Num a => Getter (Scoped a) a
localIx = to $ \sc -> sc^.scoped - sc^.locally^._1

-- sticky forward/back

forward :: (Num a, Ix a) => Scoped a -> Scoped a
forward sc
  | inRange (sc'^.locally) (sc'^.scoped) = sc'
  | inRange (sc'^.globally) (sc'^.scoped) = sc' & locally %~ (\(a,b) -> (a+1,b+1))
  | otherwise = sc' & scoped .~ 0 & locally %~ (\(a,b) -> (0,b-a)) & globally %~ (\(a,b) -> (0,b-a))
  where
    sc' = sc & scoped +~ 1

back :: (Num a, Ix a) => Scoped a -> Scoped a
back sc
  | inRange (sc'^.locally) (sc'^.scoped) = sc'
  | inRange (sc'^.globally) (sc'^.scoped) = sc' & locally %~ (\(a,b) -> (a-1,b-1))
  | otherwise = let end = sc'^.globally^._2 in sc' & scoped .~ end & locally %~ (\(a,b) -> (end-(b-a),end)) & globally %~ (\(a,b) -> (end-(b-a),end))
  where
    sc' = sc & scoped -~ 1

