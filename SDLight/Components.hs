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
  resize :: V2 Int -> t -> t

  -- constructor
  text :: String -> t
  fillRectangle :: V2 Int -> t
  shaded :: Color -> t -> t

instance Picture Component where
  translate v pic = Component $ \c -> runComponent pic c <&> _3 . _position +~ SDL.P v
  scale v pic = Component $ \c -> runComponent pic c <&> _3 . _size *~ v
  color c pic = Component $ \_ -> runComponent pic c
  resize v pic = Component $ \c -> runComponent pic c <&> _3 . _size .~ v

  text "" = error "emptyText at Component.text"
  text txt = Component $ \c -> do
    font' <- use font
    rend' <- use renderer
    with (TTF.renderUTF8Blended font' txt (toRColor c)) SDL.freeSurface $ \surface -> do
      texture <- SDL.createTextureFromSurface rend' surface
      tinfo <- lift $ SDL.queryTexture texture
      return (texture, Nothing, SDL.Rectangle (SDL.P 0) (V2 (fromEnum $ SDL.textureWidth tinfo) (fromEnum $ SDL.textureHeight tinfo)))
    where
      toRColor (Color (SDL.V4 r g b a)) = SDLR.Color (toEnum r) (toEnum g) (toEnum b) (toEnum a)

  fillRectangle v = Component $ \(Color c) -> do
    rend <- use renderer
    texture <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum v)

    with (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just texture
      SDL.rendererDrawColor rend SDL.$= fmap toEnum c
      SDL.fillRect rend (Just $ SDL.Rectangle 0 (fmap toEnum v))

    return (texture, Nothing, SDL.Rectangle 0 v)

  shaded sh pic = Component $ \c -> do
    (tex,src,tgt) <- runComponent pic c
    (texsh,_,_) <- runComponent pic sh
    
    rend <- use renderer
    texture <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum $ tgt^._size + V2 2 2)
    SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend

    with (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just texture
      SDL.copy rend texsh (fmap (fmap toEnum) src) (Just $ fmap toEnum $ SDL.Rectangle 2 (tgt^._size))
      SDL.copy rend tex (fmap (fmap toEnum) src) (Just $ fmap toEnum $ SDL.Rectangle 0 (tgt^._size))
      SDL.destroyTexture texsh
      SDL.destroyTexture tex

    return (texture, Nothing, SDL.Rectangle (tgt^._position) (tgt^._size + V2 2 2))

class Arrangement t where
  -- arrangement
  (<=>) :: t -> t -> t
  (<+>) :: t -> t -> t
  divideHin :: V2 Int -> [t] -> [t]
  divideVin :: V2 Int -> [t] -> [t]
  
instance Arrangement Component where
  pic1 <=> pic2 = Component $ \c -> do
    (tex1, src1, tgt1) <- runComponent pic1 c
    (tex2, src2, tgt2) <- runComponent pic2 c
    let siz = V2 (tgt1^._size^._x + tgt2^._size^._x) ((tgt1^._size^._y) `max` (tgt2^._size^._y))
    
    rend <- use renderer
    texture <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum siz)
    SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend

    with (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just texture
      SDL.copy rend tex1 (fmap (fmap toEnum) src1) (Just $ fmap toEnum $ SDL.Rectangle 0 (tgt1^._size))
      SDL.copy rend tex2 (fmap (fmap toEnum) src2) (Just $ fmap toEnum $ SDL.Rectangle (SDL.P $ V2 (tgt1^._size^._x) 0) (tgt2^._size))
      SDL.destroyTexture tex1
      SDL.destroyTexture tex2

    return (texture, Nothing, SDL.Rectangle (tgt1^._position) siz)

  pic1 <+> pic2 = Component $ \c -> do
    (tex1, src1, tgt1) <- runComponent pic1 c
    (tex2, src2, tgt2) <- runComponent pic2 c
    let siz = V2 ((tgt1^._size^._x) `max` (tgt2^._size^._x)) (tgt1^._size^._y + tgt2^._size^._y)
    
    rend <- use renderer
    texture <- SDL.createTexture rend SDL.RGBA8888 SDL.TextureAccessTarget (fmap toEnum siz)
    SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend

    with (SDL.get (SDL.rendererRenderTarget rend)) (\target -> SDL.rendererRenderTarget rend SDL.$= target) $ \_ -> do
      SDL.rendererRenderTarget rend SDL.$= Just texture
      SDL.copy rend tex1 (fmap (fmap toEnum) src1) (Just $ fmap toEnum $ SDL.Rectangle 0 (tgt1^._size))
      SDL.copy rend tex2 (fmap (fmap toEnum) src2) (Just $ fmap toEnum $ SDL.Rectangle (SDL.P $ V2 0 (tgt1^._size^._y)) (tgt2^._size))
      SDL.destroyTexture tex1
      SDL.destroyTexture tex2

    return (texture, Nothing, SDL.Rectangle (tgt1^._position) siz)

  divideHin area comps =
    fmap (\(i,comp) -> translate (V2 (i * area^._x `div` length comps) 0) comp) $ zip [0..] comps

  divideVin area comps =
    fmap (\(i,comp) -> translate (V2 0 (i * area^._x `div` length comps)) comp) $ zip [0..] comps

renders :: Color -> [Component] -> GameM ()
renders color xs = mapM_ (\pic -> render =<< runComponent pic color) xs
  where
    render :: (SDL.Texture, Maybe Area, Area) -> GameM ()
    render (tex,src,tgt) = do
      rend <- use renderer
      lift $ SDL.copy rend tex (fmap toEnum <$> src) (Just $ fmap toEnum tgt)
      SDL.destroyTexture tex


