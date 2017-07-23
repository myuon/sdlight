{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict, StrictData #-}
module SDLight.Widgets.FadeInOut
  ( eff'widget
  , eff'reset
  , eff'run
  , eff'switch
  , eff'fadeInOut
  , Eff'FadeInOut
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Functor.Sum
import Linear.V2
import Linear.Vector
import SDLight.Util
import SDLight.Types
import SDLight.Components
import SDLight.Widgets.Core
import SDLight.Widgets.Layer
import SDLight.Widgets.Effector

data Eff'Get w br m a where
  Eff'Get :: Eff'Get w Value Identity w

data Eff'Put w br m a where
  Eff'Put :: w -> Eff'Put w Self Identity a

data Eff'Run w br m a where
  Eff'Run :: Getter w (GameM w) -> Getter w (FreezeT w Identity ()) -> Eff'Run w Self GameM a

data Eff'Reset arg w br m a where
  Eff'Reset :: arg -> Getter w w -> Eff'Reset arg w Self Identity a

data Eff'GetAlpha w br m a where
  Eff'GetAlpha :: Eff'GetAlpha w Value Identity Double

data Eff'Switch w br m a where
  Eff'Switch :: Eff'Switch w FreezeT Identity w

type Op'FadeIn =
  [ Eff'Reset ()
  , Eff'Run
  , Eff'Switch
  , Eff'Get
  , Eff'Put
  , Eff'GetAlpha
  ]

type family (:$) (fs :: [a -> b]) (x :: a) where
  '[] :$ x = '[]
  (f : fs) :$ x = f x : fs :$ x

newtype Eff'FadeInOut w = Eff'FadeInOut { runFadeIn :: Widget (Op'FadeIn :$ w) }

instance Wrapped (Eff'FadeInOut w) where
  type Unwrapped (Eff'FadeInOut w) = Widget (Op'FadeIn :$ w)
  _Wrapped' = iso runFadeIn Eff'FadeInOut

data FadeState
  = NotReady
  | FadeIn
  | Running
  | FadeOut
  | Finished
  deriving (Eq, Show)

data Fade
  = Fade
  { _eff1 :: Widget Op'Effector
  , _eff2 :: Widget Op'Effector
  , __state :: FadeState
  }

makeLenses ''Fade

eff'widget :: Lens' (Eff'FadeInOut w) w
eff'widget = lens ((^. _value' Eff'Get) . runFadeIn) (\fw w -> Eff'FadeInOut $ runFadeIn fw ^. _self' (Eff'Put w))

eff'run :: Getter w (GameM w) -> Getter w (FreezeT w Identity ()) -> Getter (Eff'FadeInOut w) (GameM (Eff'FadeInOut w))
eff'run f g = to $ \w -> fmap Eff'FadeInOut $ runFadeIn w ^. _self (Eff'Run f g)

eff'reset :: Getter w w -> Getter (Eff'FadeInOut w) (Eff'FadeInOut w)
eff'reset f = to $ \w -> Eff'FadeInOut $ runFadeIn w ^. _self' (Eff'Reset () f)

eff'switch :: Getter (Eff'FadeInOut w) (FreezeT (Eff'FadeInOut w) Identity w)
eff'switch = to $ \w -> bimapT Eff'FadeInOut id $ runFadeIn w ^. _Op Eff'Switch

eff'fadeInOut :: Transition -> Int -> Int -> Widget xs -> Eff'FadeInOut (Widget xs)
eff'fadeInOut = \tr n1 n2 w -> Eff'FadeInOut $ go (new tr n1 n2) w where
  new :: Transition -> Int -> Int -> Fade
  new tr n1 n2 = Fade (effector tr n1) (effector (Inverse tr) n2) NotReady
  
  go :: Fade -> Widget xs -> Widget (Op'FadeIn :$ (Widget xs))
  go fade widget = Widget $
    (\(Eff'Reset _ opreset) -> continue $ reset fade widget opreset)
    @> (\(Eff'Run oprun opswitch) -> continueM $ run fade widget oprun opswitch)
    @> (\Eff'Switch -> (if fade^._state == Finished then flip freeze widget else continue) $ go fade widget)
    @> (\Eff'Get -> finish widget)
    @> (\(Eff'Put widget') -> continue $ go fade widget')
    @> (\Eff'GetAlpha -> finish $ getAlpha fade)
    @> emptyUnion

  reset :: Fade -> Widget xs -> Getter (Widget xs) (Widget xs) -> Widget (Op'FadeIn :$ (Widget xs))
  reset fade widget opreset = go fade' (widget ^. opreset) where
    fade' = fade & _state .~ NotReady & eff1 ^%~ op'reset () & eff2 ^%~ op'reset ()

  run :: Fade -> Widget xs -> Getter (Widget xs) (GameM (Widget xs)) -> Getter (Widget xs) (FreezeT (Widget xs) Identity ()) -> GameM (Widget (Op'FadeIn :$ (Widget xs)))
  run fade widget oprun opswitch = case fade^._state of
    NotReady -> return $ go (fade & _state .~ FadeIn) widget
    FadeIn ->
      fmap (\f' -> go f' widget) $ onFinishM eff1 (\e -> e^.op'run.functorial op'switch) fade $ \_ fade' -> do
        return $ fade' & _state .~ Running
    Running ->
      fmap (uncurry go) $ onFinishM _2 (\w -> w^.oprun.functorial opswitch) (fade,widget) $ \_ pair -> do
        return $ pair & _1._state .~ FadeOut
    FadeOut ->
      fmap (\f' -> go f' widget) $ onFinishM eff2 (\e -> e^.op'run.functorial op'switch) fade $ \_ fade' -> do
        return $ fade' & _state .~ Finished
    Finished -> return $ go fade widget

  getAlpha fade = case fade^._state of
    NotReady -> 0.0
    FadeIn -> fade^.eff1^.op'getValue
    Running -> 1.0
    FadeOut -> fade^.eff2^.op'getValue
    Finished -> 0.0

