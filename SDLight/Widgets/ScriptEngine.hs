{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.ScriptEngine
  ( wMiniScriptEngine
  , Op'MiniScriptEngine
  , Op'LoadMiniScript(..)
  , parseMiniScript

  , EffectIn(..)
  , EffectOut(..)
  , MiniScript
  , wait
  , destroyImage
  , withEffect
  , Position(..)
  , loadCharacter
  , showCharacter
  , speak
  , resetOpacity
  ) where
import qualified SDL as SDL
import qualified SDL.Image as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Skeleton
import Control.Applicative
import Data.List
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Linear.V2
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.MessageLayer
import qualified Text.Trifecta as Tf

newtype RefImage = RefImage Int
  deriving (Eq, Show)

data EffectIn
  = FadeInLinear Int
  | FadeInEase Int
  | CharacterIn Int
  | NoEffectIn
  deriving (Eq, Read, Show)

data EffectOut
  = FadeOutLinear Int
  | FadeOutEase Int
  | CharacterOut Int
  | NoEffectOut
  deriving (Eq, Read, Show)

effectTime :: Either EffectIn EffectOut -> Int
effectTime (Left (FadeInLinear n)) = n
effectTime (Left (FadeInEase n)) = n
effectTime (Left (CharacterIn n)) = n
effectTime (Left NoEffectIn) = 0
effectTime (Right (FadeOutLinear n)) = n
effectTime (Right (FadeOutEase n)) = n
effectTime (Right (CharacterOut n)) = n
effectTime (Right NoEffectOut) = 0

-- example

data SimpleDSL a where
  Wait :: Int -> SimpleDSL ()
  LoadImage :: FilePath -> Position -> SimpleDSL RefImage
  RenderImage :: RefImage -> SimpleDSL ()
  DestroyImage :: RefImage -> SimpleDSL ()
  RunEffect :: Either EffectIn EffectOut -> RefImage -> SimpleDSL ()
  Speak :: Maybe RefImage -> [String] -> SimpleDSL ()

  ResetOpacity :: SimpleDSL ()

type MiniScript = Skeleton SimpleDSL

wait :: Int -> MiniScript ()
wait = bone . Wait

destroyImage :: RefImage -> MiniScript ()
destroyImage = bone . DestroyImage

withEffect :: EffectIn -> EffectOut -> RefImage -> MiniScript a -> MiniScript a
withEffect ein eout ref ma = do
  bone $ RunEffect (Left ein) ref
  a <- ma
  bone $ RunEffect (Right eout) ref
  return a

data Position = L | C | R deriving (Eq, Show, Read)

loadCharacter :: FilePath -> Position -> MiniScript RefImage
loadCharacter path p = bone $ LoadImage path p

showCharacter :: RefImage -> MiniScript () -> MiniScript ()
showCharacter ref ma = do
  withEffect (CharacterIn 20) (CharacterOut 20) ref $ do
    bone $ RenderImage ref
    ma

speak :: Maybe RefImage -> [String] -> MiniScript ()
speak ref s = bone $ Speak ref s

resetOpacity :: MiniScript ()
resetOpacity = bone ResetOpacity

-- MiniScript parser

type CharaName = String

data MiniSyntax
  = SynWait Int
  | SynLoadCharacter FilePath Position CharaName
  | SynShowCharacter CharaName [MiniSyntax]
  | SynSpeak CharaName [String]
  deriving (Eq, Show)

interpret :: [MiniSyntax] -> MiniScript ()
interpret = go M.empty where
  go mp [] = return ()
  go mp (x:xs) = case x of
    SynWait n -> wait n >> go mp xs
    SynLoadCharacter path pos name -> do
      k <- loadCharacter path pos
      go (M.insert name k mp) xs
    SynShowCharacter name ms -> do
      showCharacter (mp M.! name) $ go mp ms
      go mp xs
    SynSpeak name s -> do
      speak (Just (mp M.! name)) s
      go mp xs

pminisyntax :: Tf.Parser [MiniSyntax]
pminisyntax = Tf.option [] $ Tf.many expr where
  select :: Tf.Parsing m => [m a] -> m a
  select = Tf.choice . fmap Tf.try
  
  expr = Tf.try Tf.whiteSpace
    *> select
    [ pwait Tf.<?> "@wait"
    , ploadCharacter Tf.<?> "@load"
    , pshowCharacter Tf.<?> "@show"
    , pspeak Tf.<?> "@speak"
    ]

  pwait = do
    Tf.symbol "@wait"
    SynWait . fromIntegral <$> Tf.natural
  ploadCharacter = do
    name <- Tf.some Tf.letter <* Tf.spaces
    Tf.symbol "="
    Tf.symbol "@load"
    path <- Tf.stringLiteral <* Tf.spaces
    pos <- read <$> Tf.some Tf.letter <* Tf.spaces
    return $ SynLoadCharacter path pos name
  pshowCharacter = do
    Tf.symbol "@show"
    name <- Tf.some Tf.letter <* Tf.spaces
    prog <- Tf.braces pminisyntax
    return $ SynShowCharacter name prog
  pspeak = do
    Tf.symbol "@speak"
    name <- Tf.some Tf.letter <* Tf.spaces
    s <- select [praw Tf.<?> "raw strings", plit Tf.<?> "literals"]
    return $ SynSpeak name s

    where
      plit = Tf.braces $ Tf.many Tf.stringLiteral
      praw = do
        Tf.symbolic '<'
        t <- Tf.manyTill Tf.anyChar (Tf.try (Tf.char '>'))
        return $ lines t

parseMiniScript :: FilePath -> IO (Maybe (MiniScript ()))
parseMiniScript path = fmap interpret <$> Tf.parseFromFile pminisyntax path
    
--

data Op'LoadMiniScript m r where
  Op'LoadMiniScript :: MiniScript () -> Op'LoadMiniScript Identity NoValue

type Op'MiniScriptEngine =
  [ Op'Reset '[]
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'IsFinished
  , Op'LoadMiniScript
  ]

data ScriptEngineState
  = NotReady
  | Suspended
  | Running
  | Message
  | PerformEffect (Either EffectIn EffectOut) RefImage
  | Finished
  deriving (Eq, Show)

data Layer
  = Layer
  { _texture :: SDL.Texture
  , _position :: Either Position (V2 Int)
  , _opacity :: Double
  }

makeLenses ''Layer

data ScriptEngine
  = ScriptEngine
  { __state :: ScriptEngineState
  , _layers :: IM.IntMap Layer
  , _displaying :: [RefImage]
  , _script :: MiniScript ()
  , _counter :: Int
  , _message :: Widget Op'MessageLayer
  }

makeLenses ''ScriptEngine

wMiniScriptEngine :: SDL.Texture -> V2 Int -> GameM (Widget Op'MiniScriptEngine)
wMiniScriptEngine = \texture v -> go <$> new texture v where
  new :: SDL.Texture -> V2 Int -> GameM ScriptEngine
  new texture v =
    ScriptEngine
    <$> return NotReady
    <*> return IM.empty
    <*> return []
    <*> return (return ())
    <*> return 0
    <*> wMessageLayer texture v []
  
  go :: ScriptEngine -> Widget Op'MiniScriptEngine
  go model = Widget $
    (\(Op'Reset SNil) -> continue go $ reset model)
    @> (\(Op'Render v) -> lift $ render v model)
    @> (\Op'Run -> continueM go $ run model)
    @> (\(Op'HandleEvent keys) -> continueM go $ handler keys model)
    @> (\Op'IsFinished -> finish $ model^._state == Finished)
    @> (\(Op'LoadMiniScript ms) -> continue go $ model & script .~ ms & _state .~ Running)
    @> emptyUnion

  reset :: ScriptEngine -> ScriptEngine
  reset model = model & _state .~ NotReady

  renderImage :: V2 Int -> SDL.Texture -> Double -> GameM ()
  renderImage v texture alpha = do
    rend <- use renderer
    query <- SDL.queryTexture texture
    let loc = SDL.Rectangle (SDL.P $ fmap toEnum v) (SDL.V2 (SDL.textureWidth query) (SDL.textureHeight query))
    
    alpha0 <- SDL.get $ SDL.textureAlphaMod texture
    SDL.textureAlphaMod texture SDL.$= (floor $ alpha * 255)
    lift $ SDL.copy rend texture Nothing (Just loc)
    SDL.textureAlphaMod texture SDL.$= alpha0

  toPos :: Position -> V2 Int
  toPos L = V2 0 15
  toPos C = V2 150 15
  toPos R = V2 450 15

  render :: V2 Int -> ScriptEngine -> GameM ()
  render v model = do
    forM_ (model^.displaying) $ \(RefImage ref) -> do
      let layer = (model^.layers) IM.! ref
      renderImage (v + either toPos id (layer^.position)) (layer^.texture) (layer^.opacity)

    case model^._state of
      Message -> model^.message @! Op'Render (V2 0 450)
      _ -> return ()

  findInsert :: a -> IM.IntMap a -> (Int, IM.IntMap a)
  findInsert a mp = go 0 where
    go k | k `IM.member` mp = go (k+1)
    go k = (k, IM.insert k a mp)

  uniqueInsert :: Eq a => a -> [a] -> [a]
  uniqueInsert k xs
    | k `elem` xs = xs
    | otherwise = k : xs

  run :: ScriptEngine -> GameM ScriptEngine
  run model = case model^._state of
    Suspended | model^.counter == 0 ->
      return $ model & counter .~ 0 & _state .~ Running
    Suspended -> return $ model & counter -~ 1
    Running ->
      case debone $ model^.script of
        Return () -> return $ model & _state .~ Finished
        (Wait n :>>= k) -> return $ model
          & _state .~ Suspended
          & counter .~ n
          & script .~ k ()
        (LoadImage path p :>>= k) -> do
          rend <- use renderer
          imgTexture <- lift $ SDL.loadTexture rend path
          let layer = Layer imgTexture (Left p) 0
          let (key, mp) = findInsert layer (model^.layers)
          return $ model
            & script .~ k (RefImage key)
            & layers .~ mp
        (RenderImage (RefImage ref) :>>= k) -> do
          return $ model
            & script .~ k ()
            & displaying %~ uniqueInsert (RefImage ref)
            & layers . ix ref . opacity .~ 1.0
        (DestroyImage (RefImage ref) :>>= k) -> do
          let lay = (model^.layers) IM.! ref
          SDL.destroyTexture (lay^.texture)
          return $ model
            & script .~ k ()
            & displaying %~ delete (RefImage ref)
            & layers %~ IM.delete ref
        (RunEffect eff ref :>>= k) -> do
          return $ model
            & script .~ k ()
            & _state .~ PerformEffect eff ref
            & counter .~ effectTime eff
            & displaying %~ uniqueInsert ref
        (Speak ref text :>>= k) -> do
          return $ model
            & _state .~ Message
            & script .~ k ()
            & message @%~ Op'Reset (text :. SNil)
            & layers %~ IM.mapWithKey (\key -> if Just (RefImage key) == ref then id else opacity .~ 0.5)
        (ResetOpacity :>>= k) -> do
          return $ model
            & layers %~ fmap (opacity .~ 1.0)
    Message | model^.message @@! Op'IsFinished -> return $ model & _state .~ Running
    Message -> model^.message @. Op'Run >>= \m -> return $ model & message .~ m
    PerformEffect _ _ | model^.counter <= 0 -> return $ model & _state .~ Running
    PerformEffect eff (RefImage ref) -> do
      let ratio t = (fromIntegral $ model^.counter) / fromIntegral t
      let calcOpacity :: Either EffectIn EffectOut -> Double
          calcOpacity (Left (FadeInLinear time)) = 1 - ratio time
          calcOpacity (Left (FadeInEase time)) = 1 - sin (ratio time * pi / 4)
          calcOpacity (Left (CharacterIn time)) = 1 - ratio time
          calcOpacity (Left NoEffectIn) = 1
          calcOpacity (Right (FadeOutLinear time)) = ratio time
          calcOpacity (Right (FadeOutEase time)) = sin (ratio time * pi / 4)
          calcOpacity (Right (CharacterOut time)) = sin (ratio time * pi / 4)
          calcOpacity (Right NoEffectOut) = 1
      return $ model
        & counter -~ 1
        & layers . ix ref . opacity .~ calcOpacity eff
    _ -> return model

  handler :: M.Map SDL.Scancode Int -> ScriptEngine -> GameM ScriptEngine
  handler keys model = case model^._state of
    Message -> model^.message @. Op'HandleEvent keys >>= \m -> return $ model & message .~ m
    _ -> return model
  

