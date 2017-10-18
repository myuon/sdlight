{-# LANGUAGE PolyKinds #-}
module SDLight.Util where

import qualified SDL as SDL
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Extensible
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
_position = lens (\(SDL.Rectangle a _) -> a) (\(SDL.Rectangle _ b) a' -> SDL.Rectangle a' b)

_size :: Lens' (SDL.Rectangle a) (V2 a)
_size = lens (\(SDL.Rectangle _ b) -> b) (\(SDL.Rectangle a _) b' -> SDL.Rectangle a b')

-- lens

functorial :: Functor f => Getter a b -> Getter (f a) (f b)
functorial l = to $ fmap (^.l)

monadic :: Monad m => Lens' a b -> Lens' (m a) (m b)
monadic l = lens (^. functorial l) (liftM2 (\a b -> a & l .~ b))

infixr 4 ^%~
(^%~) :: Lens' s a -> (Getter a a) -> s -> s
s ^%~ f = s %~ (^. f)

infixr 4 ^%%~
(^%%~) :: Functor m => Lens' s a -> (Getter a (m a)) -> s -> m s
s ^%%~ f = s %%~ (^. f)

infixr 4 ^%=
(^%=) :: MonadState s m => Lens' s a -> (Getter a a) -> m ()
s ^%= f = s %= (^. f)

infixr 4 ^%%=
(^%%=) :: MonadState s m => Lens' s a -> (Getter a (m a)) -> m ()
s ^%%= f = do
  x <- use s
  s <~ x ^. f

-- extensible

hmerge :: (xs ⊆ ys, Wrapper h) => h :* xs -> h :* ys -> h :* ys
hmerge hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusion) .~ x^._Wrapper) hy hx

hmergeAssoc :: (IncludeAssoc ys xs, Wrapper h) => h :* xs -> h :* ys -> h :* ys
hmergeAssoc hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusionAssoc) .~ x^._Wrapper) hy hx
