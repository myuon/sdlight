{-|
A value that has local scope and global scope
-}
module Data.Scoped where

import Control.Lens
import Data.List
import Data.Ix

-- | A closed interval
newtype Interval a = Interval { getInterval :: (a,a) }
  deriving (Eq)

makePrisms ''Interval

-- | A value scoped in local interval and global interval
data Scoped a
  = Scoped
  { _scoped :: a
  , _locally :: Interval a
  , _globally :: Interval a
  }

makeLenses ''Scoped

rangeScope :: [a] -> Int -> Maybe (Scoped Int)
rangeScope [] _ = Nothing
rangeScope xs n = Just $ scopedTo n (length xs - 1)

rangeOf :: Ix a => Scoped a -> [a]
rangeOf sc = range (getInterval $ sc^.locally) `intersect` range (getInterval $ sc^.globally)

scopedTo :: (Num a, Ord a, Show a) => a -> a -> Scoped a
scopedTo local global | 0 <= local && 0 <= global = Scoped 0 (Interval (0,local)) (Interval (0,global))
scopedTo local global = error $ show (local,global)

adjustTo0 :: Num a => Scoped a -> Scoped a
adjustTo0 sc = sc & scoped .~ 0 & locally . _Interval %~ (\(a,b) -> (0,b-a)) & globally . _Interval %~ (\(a,b) -> (0,b-a))

localIx :: Num a => Getter (Scoped a) a
localIx = to $ \sc -> sc^.scoped - sc^.locally^.to getInterval^._1

instance Ord a => Ord (Interval a) where
  Interval (a,b) <= Interval (c,d) = c <= a && b <= d

-- | Forwarding stickey
forward :: (Num a, Ix a) => Scoped a -> Scoped a
forward sc
  | inRange (getInterval $ (sc'^.locally) `min` (sc'^.globally)) (sc'^.scoped) = sc'
  | inRange (getInterval $ sc'^.globally) (sc'^.scoped) = sc' & locally . _Interval %~ (\(a,b) -> (a+1,b+1))
  | otherwise = sc' & scoped .~ 0 & locally . _Interval %~ (\(a,b) -> (0,b-a)) & globally . _Interval %~ (\(a,b) -> (0,b-a))
  where
    sc' = sc & scoped +~ 1

-- | Backwarding stickey
back :: (Num a, Ix a) => Scoped a -> Scoped a
back sc
  | inRange (getInterval $ (sc'^.locally) `min` (sc'^.globally)) (sc'^.scoped) = sc'
  | inRange (getInterval $ sc'^.globally) (sc'^.scoped) = sc' & locally . _Interval %~ (\(a,b) -> (a-1,b-1))
  | otherwise = let end = sc'^.globally^.to getInterval^._2 in sc' & scoped .~ end & locally . _Interval %~ (\(a,b) -> (end-(b-a),end)) & globally . _Interval %~ (\(a,b) -> (end-(b-a),end))
  where
    sc' = sc & scoped -~ 1

