{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight where

import qualified SDL as SDL
import qualified SDL.Raw.Types as SDLR
import qualified SDL.Internal.Numbered as SDL
import SDL.Compositor
import SDL.Compositor.Drawer (Color(..))
import qualified SDL.TTF as TTF
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.IORef
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

runGRenderer :: (Texture tex, Renderable SDL.Renderer tex) => CompositingNode tex -> GameM ()
runGRenderer node = use renderer >>= \rend -> lift $ runRenderer rend node

runGame :: s -> (s -> GameM ()) -> (s -> GameM s) -> (M.Map SDL.Scancode Int -> s -> GameM s) -> IO ()
runGame world draw step keyevent = do
  wref <- newIORef world
  
  with SDL.initializeAll (\_ -> SDL.quit) $ \_ -> do
    with (SDL.createWindow "magic labo" SDL.defaultWindow) SDL.destroyWindow $ \w' -> do
      with (SDL.createRenderer w' 0 SDL.defaultRenderer) SDL.destroyRenderer $ \r' -> do
        TTF.withInit $ do
          True <- TTF.wasInit

          with (TTF.openFont "./resources/ipag.ttf" 24) TTF.closeFont $ \font -> do
            let g0 = GameInfo w' r' font (M.fromList $ zip (fmap SDL.Scancode [0..256]) [0,0..])
            gref <- newIORef g0
            loop wref gref
      
  where
    loop wref gref = do
      game <- readIORef gref
      rendererDrawColor (game^.renderer) SDL.$= V4 255 255 255 255
      clear (game^.renderer)

      readIORef wref >>= \world ->
        evalStateT (draw world) game
      present (game^.renderer)
      SDL.delay 30

      readIORef wref >>= \world ->
        evalStateT (step world) game >>= writeIORef wref

      readIORef wref >>= \world ->
        evalStateT (keyevent (game^.keystates) world) game >>= writeIORef wref

      keys <- SDL.getKeyboardState
      writeIORef gref $ game & keystates %~ M.mapWithKey (\k v -> if keys k then v + 1 else 0)
      handler

      where
        handler = do
          SDL.pollEvent >>= \ev -> case ev of
            Just (SDL.Event _ SDL.QuitEvent) -> return ()
            z -> loop wref gref


