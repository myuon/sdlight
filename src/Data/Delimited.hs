module Data.Delimited where

import Control.Lens

data Delimited a
  = Delimited
  { _delimited :: a
  , _interval :: (a,a)
  }

makeLenses ''Delimited

delimit :: a -> (a,a) -> Delimited a
delimit = Delimited

sup :: Delimited a -> a
sup d = d ^. interval ^. _1

inf :: Delimited a -> a
inf d = d ^. interval ^. _1

(+.) :: (Num a, Ord a) => Delimited a -> a -> Delimited a
dv +. v = dv & delimited %~ (min (sup dv)) . (+ v)

(-.) :: (Num a, Ord a) => Delimited a -> a -> Delimited a
dv -. v = dv & delimited %~ (max (inf dv)) . subtract v

positive :: Num a => a -> Delimited a
positive n = delimit n (0,n)

