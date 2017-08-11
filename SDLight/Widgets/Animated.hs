module SDLight.Widgets.Animated
  ( AnimatedConfig
  , Op'Animated
  , wAnimated
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Extensible
import Data.Reflection
import Linear.V2
import SDLight.Types
import SDLight.Stylesheet
import SDLight.Widgets.Core
import SDLight.Widgets.Layer

type AnimatedConfig =
  [ "texture" >: SDL.Texture
  , "pictureSize" >: V2 Int
  ]

data Animated
  = Animated
  { _pictureSize :: V2 Int
  , _tile :: V2 Int
  , _texture :: SDL.Texture
  , _counter :: Widget Op'Delay
  }

makeLenses ''Animated

type Op'Animated = 
  [ Op'Render
  , Op'Run
  ]

wAnimated :: Given StyleSheet => WConfig AnimatedConfig -> GameM (Widget Op'Animated)
wAnimated (giveWid "animated" -> cfg) = go <$> new where
  new = do
    query <- SDL.queryTexture (cfg ^. _Wrapped . #texture)
    let sx = fromEnum $ SDL.textureWidth query
    let sy = fromEnum $ SDL.textureHeight query
    let size = V2 sx sy
    let tile = liftM2 div size (cfg ^. _Wrapped . #pictureSize)
    
    return $ Animated
      (cfg ^. _Wrapped . #pictureSize)
      tile
      (cfg ^. _Wrapped . #texture)
      (wDelay (tile ^. _x * tile ^. _y))

  go :: Animated -> Widget Op'Animated
  go model = Widget $
    (\(Op'Render alpha) -> lift $ render alpha model)
    @> (\Op'Run -> continueM $ go <$> run model)
    @> emptyUnion

  render :: Double -> Animated -> GameM ()
  render alpha model = do
    rend <- use renderer
    let c = model^.counter^.op'getCounter
    let ivx = V2 (c `mod` (model^.tile^._x)) (c `div` (model^.tile^._x))
    let tgt = SDL.Rectangle (SDL.P $ fmap toEnum (getLocation cfg)) (fmap toEnum $ model^.pictureSize)
    let src = SDL.Rectangle (SDL.P $ fmap toEnum $ (model^.pictureSize) * ivx) (fmap toEnum $ model^.pictureSize)

    alpha0 <- SDL.get $ SDL.textureAlphaMod (model^.texture)
    SDL.textureAlphaMod (model^.texture) SDL.$= (floor $ alpha * 255)
    lift $ SDL.copy rend (model^.texture) (Just src) (Just tgt)
    SDL.textureAlphaMod (model^.texture) SDL.$= alpha0

  run model = model & counter ^%%~ op'run

