{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Game where

import qualified SDL as SDL
import qualified SDL.Raw.Types as SDLR
import SDL.Compositor
import SDL.Compositor.Drawer (Color(..))
import qualified SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.IORef
import Linear.V2
import Linear.V4

data GameInfo
  = GameInfo
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , _font :: TTFFont
  }
  deriving (Eq, Show)

makeLenses ''GameInfo

type GameM = StateT GameInfo IO

with :: Monad m => m t -> (t -> m b) -> (t -> m a) -> m a
with m1 m2 m = do
  k <- m1
  r <- m k
  m2 k
  return r

data Picture
  = Blank
  | Text String

renderText :: String -> Color -> SDL.V2 Int -> GameM ()
renderText txt (Color (V4 r g b a)) pos = do
  font' <- use font
  rend' <- use renderer
  with (TTF.renderUTF8Blended font' txt (SDLR.Color r g b a)) SDL.freeSurface $ \surface -> do
    texture <- lift $ SDL.createTextureFromSurface rend' surface
    siz <- lift $ liftM2 V2 (toEnum <$> textureWidth texture) (toEnum <$> textureHeight texture)
    let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) siz
    SDL.copy rend' texture Nothing (Just loc)

runGame :: s -> (s -> GameM ()) -> (s -> GameM s) -> IO ()
runGame w draw step = do
  with (SDL.initialize [SDL.InitVideo]) (\_ -> SDL.quit) $ \_ -> do
    with (SDL.createWindow "magic labo" SDL.defaultWindow) SDL.destroyWindow $ \w' -> do
      with (SDL.createRenderer w' 0 SDL.defaultRenderer) SDL.destroyRenderer $ \r' -> do
        TTF.withInit $ do
          True <- TTF.wasInit

          with (TTF.openFont "resources/ipag.ttf" 24) TTF.closeFont $ \font -> do
            let g0 = GameInfo w' r' font
            loop g0 w
      
  where
    loop z@(GameInfo window renderer _) w = do
      rendererDrawColor renderer SDL.$= V4 255 255 255 255
      clear renderer
      runStateT (draw w) z
      present renderer
      SDL.delay 30

      w' <- evalStateT (step w) z
      handler w'

      where
        handler w' = do
          ev <- SDL.pollEvent
          case ev of
            Just (SDL.Event _ SDL.QuitEvent) -> return ()
            _ -> loop z w'
