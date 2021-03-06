{-# LANGUAGE UndecidableInstances #-}
{-|
ScriptEngine is a script engine that will load script file and manage images, texts, effects and so on
-}
module SDLight.Widgets.ScriptEngine
  (

  -- * Widget
    wMiniScriptEngine

  -- * Method
  , Op'MiniScriptEngine

  -- * Operators
  , op'loadMiniScript
  , Op'Goto
  , op'goto
  , Op'SetFields
  , op'setFields

  -- * Script
  , parseMiniScript
  , isReturn
  , (=:)

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
import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Skeleton
import Data.Extensible
import Data.Reflection
import Data.List
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import Linear.V2
import SDLight.Util ((^%~), (^%%~))
import SDLight.Types
import SDLight.Stylesheet
import SDLight.Widgets.Core
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
  Yield :: SimpleDSL ()
  Label :: Maybe String -> SimpleDSL ()

  ResetOpacity :: SimpleDSL ()

type MiniScript = Skeleton SimpleDSL

isReturn :: Skeleton t a -> Bool
isReturn m = case debone m of
  Return a -> True
  _ -> False

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

infix 2 =:
(=:) = (,)

replaceVars :: [(T.Text, T.Text)] -> MiniScript () -> MiniScript ()
replaceVars vs = hoistSkeleton go where
  go :: SimpleDSL a -> SimpleDSL a
  go (Speak m xs) = Speak m (fmap T.unpack $ foldl (\ys (v,new) -> fmap (T.replace (T.cons '$' v) new) ys) (fmap T.pack xs) vs)
  go d = d

-- MiniScript parser

type CharaName = String

data MiniSyntax
  = SynWait Int
  | SynLoadCharacter FilePath Position CharaName
  | SynShowCharacter CharaName [MiniSyntax]
  | SynSpeak (Maybe CharaName) [String]
  | SynYield
  | SynLabelled String [MiniSyntax]
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
      speak ((mp M.!) <$> name) s
      go mp xs
    SynYield -> bone Yield
    SynLabelled label ys -> do
      bone $ Label (Just label)
      go mp ys
      bone $ Label Nothing
      go mp xs

pminisyntax :: Tf.Parser [MiniSyntax]
pminisyntax = Tf.option [] $ Tf.many expr where
  select :: Tf.Parsing m => [m a] -> m a
  select = Tf.choice . fmap Tf.try

  expr :: Tf.Parser MiniSyntax
  expr = Tf.try Tf.whiteSpace
    *> select
    [ pwait Tf.<?> "@wait"
    , ploadCharacter Tf.<?> "@load"
    , pshowCharacter Tf.<?> "@show"
    , pspeak Tf.<?> "@speak"
    , pyield Tf.<?> "@yield"
    , plabel Tf.<?> "@label"
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
    name <- Tf.option Nothing (Just <$> Tf.some Tf.letter <* Tf.spaces)
    s <- select [praw Tf.<?> "raw strings", plit Tf.<?> "literals"]
    return $ SynSpeak name s
    where
      plit = Tf.braces $ Tf.many Tf.stringLiteral
      praw = do
        Tf.symbolic '<'
        t <- Tf.manyTill Tf.anyChar (Tf.try (Tf.char '>'))
        return $ lines t

  pyield = do
    Tf.symbol "@yield"
    return SynYield
  plabel = do
    Tf.symbol "@label"
    name <- Tf.some (Tf.letter <|> Tf.oneOf "-_") <* Tf.spaces
    prog <- Tf.braces pminisyntax
    return $ SynLabelled name prog

parseMiniScript :: FilePath -> IO (Maybe (MiniScript ()))
parseMiniScript path = fmap interpret <$> Tf.parseFromFile pminisyntax path
    
--

makeOp "LoadMiniScript" [t| MiniScript () -> _ Self Identity () |]
makeOp "Goto" [t| String -> _ Self GameM () |]
makeOp "SetFields" [t| [(String, String)] -> _ Self Identity () |]

type Op'MiniScriptEngine =
  [ Op'Reset ()
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'LoadMiniScript
  , Op'Goto
  , Op'SetFields
  ]

data ScriptEngineState
  = NotReady
  | Suspended
  | Running
  | Message
  | PerformEffect (Either EffectIn EffectOut) RefImage
  | Break
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
  , _label :: Maybe String
  -- TODO: これだとlabelのnestに対応できないので[String]にして構造をもたせる
  }

makeLenses ''ScriptEngine

instance Conf "script_engine" where
  type Required "script_engine" =
    [ "windowTexture" >: SDL.Texture
    , "clickwaitConfig" >: Record (Required "animated")
    ]

  type Optional "script_engine" = '[ "size" >: V2 Int ]
  def = shrinkAssoc $ def @"message_layer"

wMiniScriptEngine :: Given StyleSheet => WConfig "script_engine" -> GameM (Widget Op'MiniScriptEngine)
wMiniScriptEngine (wconf #script_engine -> ViewWConfig wix req opt) = go <$> new where
  new :: GameM ScriptEngine
  new =
    ScriptEngine
    <$> return NotReady
    <*> return IM.empty
    <*> return []
    <*> return (return ())
    <*> return 0
    <*> wMessageLayer (conf @"message_layer" wix req (def @"message_layer"))
    <*> return Nothing

  go :: ScriptEngine -> Widget Op'MiniScriptEngine
  go model = Widget $
    (\(Op'Reset _) -> continue $ go $ reset model)
    @> (\(Op'Render _) -> lift $ render (given ^. wlocation wix) model)
    @> (\Op'Run -> continueM $ fmap go $ run model)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys model)
    @> (\Op'Switch -> switch model)
    @> (\(Op'LoadMiniScript ms) -> continue $ go $ model & script .~ ms & _state .~ Running)
    @> (\(Op'Goto lbl) -> continueM $ fmap go $ skipUntil lbl model)
    @> (\(Op'SetFields fs) -> continue $ go $ model & script %~ replaceVars (fmap (\(a,b) -> (T.pack a, T.pack b)) fs))
    @> emptyUnion

  reset :: ScriptEngine -> ScriptEngine
  reset model = model & _state .~ NotReady

  skipUntil :: String -> ScriptEngine -> GameM ScriptEngine
  skipUntil lbl model = go model where
    go s = do
      s' <- run s
      if s'^.label == Just lbl then return s else go s'

  switch model = case model^._state of
    Finished -> freeze' $ go model
    Break -> freeze' $ go $ model & _state .~ Running
    _ -> continue $ go model

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
      Message -> model^.message^.op'render
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
        Return () -> (lift (print "return") >>) $ return $ model & _state .~ Finished
        (Yield :>>= k) -> (lift (print "yield") >>) $ return $ model & _state .~ Break & script .~ k ()
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
          (lift (print "speak") >>) $ return $ model
            & _state .~ Message
            & script .~ k ()
            & message ^%~ op'reset text
            & layers %~ IM.mapWithKey (\key -> if Just (RefImage key) == ref then opacity .~ 1 else opacity .~ 0.5)
        (ResetOpacity :>>= k) -> return $ model & layers %~ fmap (opacity .~ 1.0)
        (Label lb :>>= k) -> (lift (print "label") >>) $ run $ model & label .~ lb & script .~ k ()
    Message | op'isFreeze (model^.message) op'switch -> return $ model & _state .~ Running
    Message -> model & message ^%%~ op'run
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
    Message -> model & message ^%%~ op'handleEvent keys
    _ -> return model
  

