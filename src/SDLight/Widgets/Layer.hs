{-|
A widget with layer
-}
module SDLight.Widgets.Layer
  (
  -- * Widget
    wLayer
  , wDelay

  -- * Method
  , Op'Layer
  , Op'Delay

  -- * Operator
  , op'renderLayer
  , op'getCounter

  -- * Combinator
  , Layer
  , newLayer
  , resizeLayer
  ) where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import qualified Data.Map as M
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Reader
import Data.Reflection
import Data.Extensible
import Linear.V2
import SDLight.Types
import SDLight.Stylesheet
import SDLight.Widgets.Core

-- | Layer datatype
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

-- | Construct a layer from texture (consists of 3x3 parts) and size. This functon does not destroy given texture.
newLayer :: SDL.Texture -> V2 Int -> GameM Layer
newLayer imgTexture (V2 width height) = do
  layer <- resizeLayer imgTexture width height
  return layer

-- | Resize layer with given width and height
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

-- | Render a layer
op'renderLayer :: V2 Int -> Double -> Getter Layer (GameM ())
op'renderLayer pos alpha = to $ \layer -> do
  rend <- use renderer
  let loc = SDL.Rectangle (SDL.P $ fmap toEnum pos) (SDL.V2 (layer^.layerWidth) (layer^.layerHeight))

  alpha0 <- SDL.get $ SDL.textureAlphaMod (layer^.layerTexture)
  SDL.textureAlphaMod (layer^.layerTexture) SDL.$= (floor $ alpha * 255)
  lift $ SDL.copy rend (layer^.layerTexture) Nothing (Just $ fmap toEnum $ loc)
  SDL.textureAlphaMod (layer^.layerTexture) SDL.$= alpha0

-- | Method of 'wLayer'
type Op'Layer =
  '[ Op'Render
  ]

instance Conf "layer" where
  type Required "layer" =
    [ "windowTexture" >: SDL.Texture
    , "size" >: V2 Int
    ]

  type Optional "layer" = '[]
  def = emptyRecord

-- | Layer widget
--
-- == Config Parameter
-- === Required
--
-- @
-- [ "windowTexture" >: SDL.Texture  -- Texture of layer
-- , "size" >: V2 Int  -- Size of this widget
-- ]
-- @
--
-- === Optional
--
-- @
-- []
-- @
--
-- == Methods
--
-- * 'op'render' Render operator
--
wLayer :: Given StyleSheet => WConfig "layer" -> GameM (NamedWidget Op'Layer)
wLayer (giveWid #layer -> cfg) = wNamed (cfg ^. #wix) . go <$> new where
  new = newLayer (cfg ^. #required ^. #windowTexture) (cfg ^. #required ^. #size)
  
  go :: Layer -> Widget Op'Layer
  go layer = Widget $
    (\(Op'Render alpha) -> lift $ layer ^. op'renderLayer (getLocation (cfg ^. #wix)) alpha)
    @> emptyUnion

-- | Layer widget and its texture will be loaded using given filepath
wLayerFilePath :: Given StyleSheet => FilePath -> WConfig "layer" -> GameM (NamedWidget Op'Layer)
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

-- | Method of 'wDelay'
type Op'Delay =
  [ Op'Reset ()
  , Op'Run
  , Op'GetCounter
  ]

-- | Delay widget, this widget has a looped counter
--
-- == Methods
--
-- * 'op'reset' Reset operator
-- * 'op'run' Run operator, this increments the counter
-- * 'op'getCounter' Get the current counter
--
wDelay :: Int -> Widget Op'Delay
wDelay = \n -> go (Delay 0 n) where
  go :: Delay -> Widget Op'Delay
  go delay = Widget $
    (\(Op'Reset _) -> continue $ go $ delay & counter .~ 0)
    @> (\Op'Run -> continueM $ fmap go $ run delay)
    @> (\Op'GetCounter -> finish $ delay^.counter)
    @> emptyUnion

  run delay = return $ delay & counter %~ (`mod` delay^.delayCount) . (+1)

