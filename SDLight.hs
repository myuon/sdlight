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
import qualified SDL.TTF as TTF
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.IORef
import Linear.V4
import SDLight.Types

data Delayed a
  = Delayed
  { _delayed :: a
  , _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delayed

newDelayed :: Int -> a -> Delayed a
newDelayed n ma = Delayed ma 0 n

runDelayed :: (a -> GameM a) -> Delayed a -> GameM (Delayed a)
runDelayed ma delay = do
  ma' <- if delay^.counter == 0 then ma (delay^.delayed) else return (delay^.delayed)
  return $ delay & delayed .~ ma'
                 & counter %~ (`mod` (delay^.delayCount)) . (+1)

runGRenderer :: (Texture tex, Renderable SDL.Renderer tex) => CompositingNode tex -> GameM ()
runGRenderer node = use renderer >>= \rend -> lift $ runRenderer rend node

runGame
  :: GameM s -- initial state
  -> (s -> GameM ()) -- draw
  -> (s -> GameM s) -- update
  -> (M.Map SDL.Scancode Int -> s -> GameM s) -- key event handler
  -> IO ()
runGame initialize draw step keyevent = do
  with SDL.initializeAll (\_ -> SDL.quit) $ \_ -> do
    with (SDL.createWindow "magic labo" SDL.defaultWindow) SDL.destroyWindow $ \w' -> do
      with (SDL.createRenderer w' 0 SDL.defaultRenderer) SDL.destroyRenderer $ \r' -> do
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


