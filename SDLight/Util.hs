{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SDLight.Util where

import qualified SDL as SDL
import Control.Lens
import Data.Proxy
import GHC.TypeLits
import Linear.V2
import Linear.V4

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


