{-|
datatype of Delimited value in a interval
-}
module Data.Delimited where

import Control.Lens

-- | @ Delimited x (a,b) @ means x is in interval [a,b]
data Delimited a
  = Delimited
  { _delimited :: a    -- ^ value
  , _interval :: (a,a)    -- ^ range of its value
  }

makeLenses ''Delimited

-- | Delimited constructor
delimit :: a -> (a,a) -> Delimited a
delimit = Delimited

-- | > sup (delimit x (a,b)) = a
sup :: Delimited a -> a
sup d = d ^. interval ^. _1

-- | > inf (delimit x (a,b)) = b
inf :: Delimited a -> a
inf d = d ^. interval ^. _1

-- | addition
(+.) :: (Num a, Ord a) => Delimited a -> a -> Delimited a
dv +. v = dv & delimited %~ (min (sup dv)) . (+ v)

-- | subtraction
(-.) :: (Num a, Ord a) => Delimited a -> a -> Delimited a
dv -. v = dv & delimited %~ (max (inf dv)) . subtract v

-- | > positive n = 'delimit' n (0,n)
positive :: Num a => a -> Delimited a
positive n = delimit n (0,n)

