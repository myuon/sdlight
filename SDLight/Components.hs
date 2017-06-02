{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Components where

import qualified SDL as SDL
import qualified SDL.Raw.Types as SDLR
import qualified SDL.TTF as TTF
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Linear.V2
import SDLight.Types
import SDLight.Util
import SDLight.Widgets.Layer

type Area = SDL.Rectangle Int

newtype Component = Component {
  runComponent :: Color -> GameM (SDL.Texture, Maybe Area, Area) }

class Picture t where
  -- operation
  translate :: V2 Int -> t -> t
  scale :: V2 Int -> t -> t
  color :: Color -> t -> t

  -- constructor
  text :: String -> t

instance Picture Component where
  translate v pic = Component $ \c -> runComponent pic c <&> _3 . _position +~ SDL.P v
  scale v pic = Component $ \c -> runComponent pic c <&> _3 . _size *~ v
  color c pic = Component $ \_ -> runComponent pic c

  text txt = Component $ \c -> do
    font' <- use font
    rend' <- use renderer
    with (TTF.renderUTF8Blended font' txt (toRColor c)) SDL.freeSurface $ \surface -> do
      texture <- SDL.createTextureFromSurface rend' surface
      tinfo <- lift $ SDL.queryTexture texture
      return (texture, Nothing, SDL.Rectangle (SDL.P 0) (V2 (fromEnum $ SDL.textureWidth tinfo) (fromEnum $ SDL.textureHeight tinfo)))
    where
      toRColor (Color (SDL.V4 r g b a)) = SDLR.Color (toEnum r) (toEnum g) (toEnum b) (toEnum a)

class Pictures t where
  -- arrangement
  (<=>) :: t -> t -> [t]
  (<+>) :: t -> t -> [t]

instance Pictures Component where
  pic1 <=> pic2 = [pic1, pic2'] where
    pic2' = Component $ \c -> do
      p <- runComponent pic1 c
      runComponent (translate (V2 (p^._3^._size^._x) 0) pic2) c
  pic1 <+> pic2 = [pic1, pic2'] where
    pic2' = Component $ \c -> do
      p <- runComponent pic1 c
      runComponent (translate (V2 0 (p^._3^._size^._y)) pic2) c

renders :: [Component] -> Color -> GameM ()
renders xs color = mapM_ (\pic -> render =<< runComponent pic color) xs
  where
    render :: (SDL.Texture, Maybe Area, Area) -> GameM ()
    render (tex,src,tgt) = use renderer >>= \r -> lift $ SDL.copy r tex (fmap toEnum <$> src) (Just $ fmap toEnum tgt)


