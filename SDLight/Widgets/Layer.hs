{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Layer where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import SDL.Compositor
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Linear.V2
import SDLight.Types

data Layer
  = Layer
  { _layerWidth :: Int
  , _layerHeight :: Int
  , _layerTexture :: SDL.Texture
  }

makeLenses ''Layer

{-
source/target

<----> sx
                  V
| LT | CT | RT |  | sy
----------------  ^
|  L |  C |  R |
----------------
| LB | CB | RB |

-}

newLayer :: FilePath -> Int -> Int -> GameM Layer
newLayer path width height = do
  rend <- use renderer
  imgTexture <- lift $ SDL.loadTexture rend path
  imgW <- lift $ textureWidth imgTexture
  imgH <- lift $ textureHeight imgTexture
  let sx = imgW `div` 3
  let sy = imgH `div` 3
  let ssiz = V2 sx sy
  let sourceLoc = M.fromList [(fromEnum $ ix+iy*3, SDL.Rectangle (SDL.P $ V2 (sx*ix) (sy*iy)) ssiz) | ix <- [0..2], iy <- [0..2]]
  let targetLoc = M.fromList [(fromEnum $ ix+iy*3, SDL.Rectangle (SDL.P $ V2 (if ix/=2 then sx*ix else width-sx) (if iy/=2 then sy*iy else height-sy)) (V2 (if ix/=1 then sx else width-sx*2) (if iy/=1 then sy else height-sy*2))) | ix <- [0..2], iy <- [0..2]]

  emptyTexture <- lift $ createTexture @SDL.Renderer @SDL.Texture rend SDL.ARGB8888 SDL.TextureAccessTarget (fmap toEnum $ V2 width height)
  rendererRenderTarget rend SDL.$= Just emptyTexture
  forM_ [ix+iy*3 | ix <- [0..2], iy <- [0..2]] $ \loc ->
    lift $ copyEx rend imgTexture (Just $ sourceLoc M.! loc) (Just $ targetLoc M.! loc) 0 Nothing (V2 False False)
  rendererRenderTarget @SDL.Renderer @SDL.Texture rend SDL.$= Nothing
  
  return $ Layer width height emptyTexture

renderLayer :: Layer -> SDL.V2 Int -> GameM ()
renderLayer layer pos = do
  rend <- use renderer
  let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) (SDL.V2 (layer^.layerWidth) (layer^.layerHeight))
  lift $ copyEx rend (layer^.layerTexture) Nothing (Just loc) 0 Nothing (V2 False False)

newtype Layered a = Layered (a, Layer)

_layered :: Lens' (Layered a) a
_layered = lens (\(Layered (c,_)) -> c) (\(Layered (_,l)) c' -> Layered (c',l))

newLayered :: FilePath -> Int -> Int -> GameM a -> GameM (Layered a)
newLayered path w h initA = Layered <$> liftM2 (,) initA (newLayer path w h)

renderLayered :: Layered a -> V2 Int -> (a -> GameM ()) -> GameM ()
renderLayered (Layered (ma,layer)) pos k = renderLayer layer pos >> k ma

