{-|
Useful Types
-}
module SDLight.Types where

import qualified SDL as SDL
import SDL.TTF.FFI (TTFFont)
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.Map as M

-- | Represents internal information
data GameInfo
  = GameInfo
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , _font :: TTFFont
  , _keystates :: M.Map SDL.Scancode Int
  }
  deriving (Eq, Show)

makeLenses ''GameInfo

-- | Monadic combinator with open/finish functions
with :: Monad m => m t -> (t -> m b) -> (t -> m a) -> m a
with m1 m2 m = do
  k <- m1
  r <- m k
  m2 k
  return r

-- | GameM monad
type GameM = StateT GameInfo IO

-- | Event handler combinator with lens
handleEventLensed :: a -> Lens' a b -> (ev -> b -> GameM b) -> ev -> GameM a
handleEventLensed v target handleEvent ev = do
  newB <- handleEvent ev (v^.target)
  return $ v & target .~ newB

-- | Run combinator with lens
runLensed :: a -> Lens' a b -> (b -> GameM b) -> GameM a
runLensed v target handler = do
  newB <- handler (v^.target)
  return $ v & target .~ newB

