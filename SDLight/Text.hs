module SDLight.Text where

import qualified SDL as SDL
import qualified SDL.Raw.Types as SDLR
import SDL.Compositor
import SDL.Compositor.Drawer (Color(..))
import qualified SDL.TTF as TTF
import Control.Lens
import Control.Monad.State
import Linear.V2
import Linear.V4
import SDLight.Types

renderText :: String -> Color -> SDL.V2 Int -> GameM ()
renderText txt (Color (V4 r g b a)) pos = do
  font' <- use font
  rend' <- use renderer
  with (TTF.renderUTF8Blended font' txt (SDLR.Color r g b a)) SDL.freeSurface $ \surface -> do
    texture <- lift $ SDL.createTextureFromSurface rend' surface
    siz <- lift $ liftM2 V2 (toEnum <$> textureWidth texture) (toEnum <$> textureHeight texture)
    let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) siz
    SDL.copy rend' texture Nothing (Just loc)

