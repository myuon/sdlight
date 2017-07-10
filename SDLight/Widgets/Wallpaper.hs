{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Wallpaper where

import qualified SDL as SDL
import qualified SDL.Image as SDL
import Control.Lens
import Control.Monad.Trans
import qualified Data.Map as M
import Linear.V2
import SDLight.Widgets.Effector
import SDLight.Widgets.Core
import SDLight.Types

data WallpaperState
  = Running
  | Finished

data Wallpaper
  = Wallpaper
  { _texture :: SDL.Texture
  , __state :: WallpaperState
  }

makeLenses ''Wallpaper

type Op'Wallpaper =
  [ Op'Reset '[]
  , Op'Render
  , Op'HandleEvent
  ]

wWallpaper :: FilePath -> GameM (Widget Op'Wallpaper)
wWallpaper = \path -> go <$> new path where
  new :: FilePath -> GameM Wallpaper
  new path
    = Wallpaper
    <$> (use renderer >>= \r -> SDL.loadTexture r path)
    <*> return Running

  go :: Wallpaper -> Widget Op'Wallpaper
  go model = Widget $
    (\(Op'Reset _) -> continue go $ reset model)
    @> (\(Op'Render v) -> lift $ render v model)
    @> (\(Op'HandleEvent keys) -> continueM go $ handler keys model)
    @> emptyUnion

  reset model = model & _state .~ Running

  render :: V2 Int -> Wallpaper -> GameM ()
  render v model = case model^._state of
    Running -> do
      rend <- use renderer
      query <- SDL.queryTexture (model^.texture)
      let loc = SDL.Rectangle (SDL.P $ fmap toEnum v) (V2 (SDL.textureWidth query) (SDL.textureHeight query))
      lift $ SDL.copy rend (model^.texture) Nothing (Just loc)
    Finished -> return ()

  handler keys model
    | keys M.! SDL.ScancodeZ == 1 = return $ model & _state .~ Finished
    | otherwise = return model

