{-|
Wallpaper will display a picture on whole screen
-}
module SDLight.Widgets.Wallpaper
  (
  -- * Widget
    wWallpaper

  -- * Method
  , Op'Wallpaper
  ) where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import Control.Lens
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Extensible
import Data.Reflection
import Linear.V2
import SDLight.Types
import SDLight.Stylesheet
import SDLight.Widgets.Core

data WallpaperState
  = Running
  | Finished

data Wallpaper
  = Wallpaper
  { _texture :: SDL.Texture
  , __state :: WallpaperState
  }

makeLenses ''Wallpaper

-- | Method of 'wWallpaper'
type Op'Wallpaper =
  [ Op'Reset ()
  , Op'Render
  , Op'HandleEvent
  ]

instance Conf "wallpaper" where
  type Required "wallpaper" =
    '[ "bgfile" >: FilePath
     ]
  type Optional "wallpaper" = '[]

  def = emptyRecord

wWallpaper :: Given StyleSheet => WConfig "wallpaper" -> GameM (Widget Op'Wallpaper)
wWallpaper (wconf #wallpaper -> ViewWConfig wix req opt) = go <$> new where
  new :: GameM Wallpaper
  new = Wallpaper
    <$> (use renderer >>= \r -> SDL.loadTexture r (req ^. #bgfile))
    <*> return Running

  go :: Wallpaper -> Widget Op'Wallpaper
  go model = Widget $
    (\(Op'Reset _) -> continue $ go $ reset model)
    @> (\(Op'Render _) -> lift $ render model)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys model)
    @> emptyUnion

  reset model = model & _state .~ Running

  render :: Wallpaper -> GameM ()
  render model = case model^._state of
    Running -> do
      rend <- use renderer
      query <- SDL.queryTexture (model^.texture)
      let loc = SDL.Rectangle (SDL.P $ fmap toEnum (getLocation wix)) (V2 (SDL.textureWidth query) (SDL.textureHeight query))
      lift $ SDL.copy rend (model^.texture) Nothing (Just loc)
    Finished -> return ()

  handler keys model
    | keys M.! SDL.ScancodeZ == 1 = return $ model & _state .~ Finished
    | otherwise = return model

