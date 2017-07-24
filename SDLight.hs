{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight where

import qualified SDL as SDL
import qualified SDL.TTF as TTF
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.IORef
import Linear.V4
import SDLight.Types

runGame
  :: GameM s -- initial state
  -> (s -> GameM ()) -- draw
  -> (s -> GameM s) -- update
  -> (M.Map SDL.Scancode Int -> s -> GameM s) -- key event handler
  -> IO ()
runGame initialize draw step keyevent = do
  with SDL.initializeAll (\_ -> SDL.quit) $ \_ -> do
    with (SDL.createWindow "magic labo" SDL.defaultWindow) SDL.destroyWindow $ \w' -> do
      with (SDL.createRenderer w' (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \r' -> do
        TTF.withInit $ do
          True <- TTF.wasInit

          with (TTF.openFont "./resources/ipag.ttf" 24) TTF.closeFont $ \font -> do
            let g0 = GameInfo w' r' font (M.fromList $ zip (fmap SDL.Scancode [0..256]) [0,0..])
            gref <- newIORef g0
            wref <- newIORef =<< evalStateT initialize g0
            loop wref gref
      
  where
    loop wref gref = do
      game <- readIORef gref
      SDL.rendererDrawColor (game^.renderer) SDL.$= V4 255 255 255 255
      SDL.clear (game^.renderer)

      readIORef wref >>= \world ->
        evalStateT (draw world) game
      SDL.present (game^.renderer)

      readIORef wref >>= \world ->
        evalStateT (step world) game >>= writeIORef wref

      readIORef wref >>= \world ->
        evalStateT (keyevent (game^.keystates) world) game >>= writeIORef wref

      keys <- SDL.getKeyboardState
      writeIORef gref $ game & keystates %~ M.mapWithKey (\k v -> if keys k then v + 1 else 0)
      handler

      where
        handler = do
          SDL.waitEventTimeout 33 >>= \ev -> case ev of
            Just (SDL.Event _ SDL.QuitEvent) -> return ()
            Just (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym SDL.ScancodeEscape _ _)))) -> return ()
            Just z@(SDL.Event _ (SDL.SysWMEvent _)) -> error $ show z
            _ -> loop wref gref


