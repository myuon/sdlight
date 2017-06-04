{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module SDLight.Util where

import qualified SDL as SDL
import Control.Lens
import Data.Proxy
import GHC.TypeLits
import Linear.V2
import Linear.V4

-- State Management

class HasState c a | c -> a where
  _state :: Lens' c a

data SymbolOf (xs :: [Symbol]) = SymbolOf_ SomeSymbol
data SymbolsOf (xs :: [[Symbol]]) = SymbolsOf_ [SomeSymbol]

class Elem (s :: k) (xs :: [k])
instance Elem s (s : xs)
instance {-# Overlappable #-} Elem s xs => Elem s (t : xs)

symbolOf :: SymbolOf xs -> String
symbolOf (SymbolOf_ (SomeSymbol t)) = symbolVal t

inj :: (Elem s xs, KnownSymbol s) => Proxy s -> SymbolOf xs
inj p = SymbolOf_ $ SomeSymbol p

symbolsOf :: SymbolsOf xs -> [String]
symbolsOf (SymbolsOf_ xs) = fmap (\(SomeSymbol p) -> symbolVal p) xs

injs :: (Elem s xs, ToSymbolList s) => Proxy s -> SymbolsOf xs
injs p = SymbolsOf_ $ toSymbolList p

class ToSymbolList (xs :: [Symbol]) where
  toSymbolList :: Proxy xs -> [SomeSymbol]
instance ToSymbolList '[] where
  toSymbolList _ = []
instance (ToSymbolList xs, KnownSymbol x) => ToSymbolList (x : xs) where
  toSymbolList p =
    let (p1,p2) = splitCons p in SomeSymbol p1 : toSymbolList p2

splitCons :: KnownSymbol x => Proxy (x : xs) -> (Proxy x, Proxy xs)
splitCons Proxy = (Proxy, Proxy)

-- color

class RGBA c where
  rgba :: Int -> Int -> Int -> Int -> c
  rgb :: Int -> Int -> Int -> c
  opacity :: Int -> c -> c

newtype Color = Color (V4 Int)

getColor :: Color -> V4 Int
getColor (Color v) = v

instance RGBA Color where
  rgba r g b a = Color $ V4 r g b a
  rgb r g b = Color $ V4 r g b 255
  opacity a (Color c) = Color $ c & _w .~ a

black :: Color
black = rgba 0 0 0 255

white :: Color
white = rgba 255 255 255 255

red :: Color
red = rgba 255 0 0 255

green :: Color
green = rgba 0 255 0 255

blue :: Color
blue = rgba 0 0 255 255

orange :: Color
orange = rgba 255 128 0 255

-- rectangle

_position :: Lens' (SDL.Rectangle a) (SDL.Point V2 a)
_position = lens (\(SDL.Rectangle a b) -> a) (\(SDL.Rectangle _ b) a' -> SDL.Rectangle a' b)

_size :: Lens' (SDL.Rectangle a) (V2 a)
_size = lens (\(SDL.Rectangle a b) -> b) (\(SDL.Rectangle a _) b' -> SDL.Rectangle a b')


