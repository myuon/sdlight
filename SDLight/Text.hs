module SDLight.Text where

import qualified SDL as SDL
import qualified SDL.Raw.Types as SDLR
import qualified SDL.TTF as TTF
import Control.Lens
import Control.Monad.State
import Linear.V2
import Linear.V4
import SDLight.Types
import SDLight.Util
import SDLight.Components

{-
renderText :: String -> Color -> SDL.V2 Int -> GameM ()
renderText txt (Color (V4 r g b a)) pos = do
  font' <- use font
  rend' <- use renderer
  with (TTF.renderUTF8Blended font' txt (SDLR.Color r g b a)) SDL.freeSurface $ \surface -> do
    texture <- lift $ SDL.createTextureFromSurface rend' surface
    query <- lift $ SDL.queryTexture texture
    siz <- lift $ liftM2 V2 (SDL.textureWidth query) (SDL.textureHeight query)
    let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) siz
    SDL.copy rend' texture Nothing (Just loc)
-}

renderText :: String -> Color -> SDL.V2 Int -> GameM ()
renderText txt color v = renders color [translate v $ text txt]

renderShadedText :: String -> Color -> Color -> SDL.V2 Int -> GameM ()
renderShadedText txt frontc backc pos = do
  renderText txt backc (pos + 2)
  renderText txt frontc pos

