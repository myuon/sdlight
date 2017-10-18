module SDLight.Types where

import qualified SDL as SDL
import SDL.TTF.FFI (TTFFont)
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.Map as M

data GameInfo
  = GameInfo
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , _font :: TTFFont
  , _keystates :: M.Map SDL.Scancode Int
  }
  deriving (Eq, Show)

makeLenses ''GameInfo

with :: Monad m => m t -> (t -> m b) -> (t -> m a) -> m a
with m1 m2 m = do
  k <- m1
  r <- m k
  m2 k
  return r

type GameM = StateT GameInfo IO

handleEventLensed :: a -> Lens' a b -> (ev -> b -> GameM b) -> ev -> GameM a
handleEventLensed v target handleEvent ev = do
  newB <- handleEvent ev (v^.target)
  return $ v & target .~ newB

runLensed :: a -> Lens' a b -> (b -> GameM b) -> GameM a
runLensed v target handler = do
  newB <- handler (v^.target)
  return $ v & target .~ newB

