{-# LANGUAGE IncoherentInstances #-}
module SDLight.Widgets.Layer
  ( wLayer
  , Op'Layer

  , wDelay
  , Op'Delay
  , op'getCounter

  , LayerConfig
  ) where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import qualified Data.Map as M
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Reader
import Data.Extensible
import Data.Default
import Linear.V2
import SDLight.Types
import SDLight.Stylesheet
import SDLight.Widgets.Core

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

-- Now, this functon does *not* destroy given texture
newLayer :: SDL.Texture -> V2 Int -> GameM Layer
newLayer imgTexture (V2 width height) = do
  layer <- resizeLayer imgTexture width height
  return layer

resizeLayer :: SDL.Texture -> Int -> Int -> GameM Layer
resizeLayer imgTexture width height = do
  rend <- use renderer
  imgQuery <- SDL.queryTexture imgTexture
  let sx = fromEnum $ SDL.textureWidth imgQuery `div` 3
  let sy = fromEnum $ SDL.textureHeight imgQuery `div` 3
  let ssiz = V2 sx sy
  let sourceLoc = M.fromList [(fromEnum $ ix+iy*3, SDL.Rectangle (SDL.P $ V2 (sx*ix) (sy*iy)) ssiz) | ix <- [0..2], iy <- [0..2]]
  let targetLoc = M.fromList [(fromEnum $ ix+iy*3, SDL.Rectangle (SDL.P $ V2 (if ix/=2 then sx*ix else width-sx) (if iy/=2 then sy*iy else height-sy)) (V2 (if ix/=1 then sx else width-sx*2) (if iy/=1 then sy else height-sy*2))) | ix <- [0..2], iy <- [0..2]]

  emptyTexture <- lift $ SDL.createTexture rend SDL.ARGB8888 SDL.TextureAccessTarget (fmap toEnum $ V2 width height)
  SDL.textureBlendMode emptyTexture SDL.$= SDL.BlendAlphaBlend
  
  SDL.rendererRenderTarget rend SDL.$= Just emptyTexture
  forM_ [ix+iy*3 | ix <- [0..2], iy <- [0..2]] $ \loc ->
    lift $ SDL.copy rend imgTexture (Just $ fmap toEnum $ sourceLoc M.! loc) (Just $ fmap toEnum $ targetLoc M.! loc)
  SDL.rendererRenderTarget rend SDL.$= Nothing
    
  return $ Layer width height emptyTexture

renderLayer :: Layer -> V2 Int -> Double -> GameM ()
renderLayer layer pos alpha = do
  rend <- use renderer
  let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) (SDL.V2 (layer^.layerWidth) (layer^.layerHeight))

  alpha0 <- SDL.get $ SDL.textureAlphaMod (layer^.layerTexture)
  SDL.textureAlphaMod (layer^.layerTexture) SDL.$= (floor $ alpha * 255)
  lift $ SDL.copy rend (layer^.layerTexture) Nothing (Just $ fmap toEnum $ loc)
  SDL.textureAlphaMod (layer^.layerTexture) SDL.$= alpha0

type Op'Layer =
  '[ Op'Render
  ]

type LayerConfig =
  [ "windowTexture" >: SDL.Texture
  , "size" >: V2 Int
  ]

instance Default (Config LayerConfig) where
  def = Config $
    #windowTexture @= error "not initialized"
    <: #size @= V2 100 100
    <: emptyRecord

wLayer :: WConfig LayerConfig -> GameM (NamedWidget Op'Layer)
wLayer (Config cfg) = wNamed wid . go <$> newLayer (cfg ^. #windowTexture) (cfg ^. #size) where
  wid = (cfg ^. #wix </> WId "layer")
  
  go :: Layer -> Widget Op'Layer
  go layer = Widget $
    (\(Op'Render alpha v) -> lift $ renderLayer layer v alpha)
    @> emptyUnion

wLayerFilePath :: FilePath -> WConfig LayerConfig -> GameM (NamedWidget Op'Layer)
wLayerFilePath path cfg = do
  rend <- use renderer
  texture <- SDL.loadTexture rend path
  wLayer cfg

-- Delayed

data Delay
  = Delay
  { _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delay

makeOp "GetCounter" [t| _ Value Identity Int |]

type Op'Delay =
  [ Op'Reset ()
  , Op'Run
  , Op'GetCounter
  ]

wDelay :: Int -> Widget Op'Delay
wDelay = \n -> go (Delay 0 n) where
  go :: Delay -> Widget Op'Delay
  go delay = Widget $
    (\(Op'Reset _) -> continue $ go $ delay & counter .~ 0)
    @> (\Op'Run -> continueM $ fmap go $ run delay)
    @> (\Op'GetCounter -> finish $ delay^.counter)
    @> emptyUnion

  run delay = return $ delay & counter %~ (`mod` delay^.delayCount) . (+1)
  

