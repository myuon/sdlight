{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module SDLight.Widgets.Core
  ( Op'Render(..)
  , op'render
  , op'renderAlpha
  , Op'Run(..)
  , op'run
  , Op'Reset(..)
  , op'reset
  , Op'HandleEvent(..)
  , op'handleEvent
  , Op'Switch(..)
  , op'switch

  , override
  , type (++)

  , runSwitch
  , runSwitchM
  , op'isFreeze

  , WConfig
  , Conf(..)
  , ViewWConfig(..)

  , giveWid
  , getLocation
  , mkCfg
  , viewWConfig
  , wconf
  , conf

  , module M
  ) where

import qualified SDL as SDL
import Control.Lens
import qualified Data.Map as M
import Data.Functor.Sum
import Data.Reflection
import Data.Extensible
import Data.Proxy
import GHC.TypeLits
import SDLight.Types
import SDLight.Stylesheet
import SDLight.Widgets.Internal.Widget as M
import SDLight.Widgets.Internal.TH as M
import SDLight.Widgets.Internal.Named as M
import SDLight.Widgets.Internal.Freeze as M

data Op'Render br m r where
  Op'Render :: Double -> Op'Render Value GameM ()

data Op'Run br m r where
  Op'Run :: Op'Run Self GameM a

data Op'Reset arg br m r where
  Op'Reset :: arg -> Op'Reset arg Self Identity a

data Op'HandleEvent br m r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent Self GameM a

data Op'Switch br m r where
  Op'Switch :: Op'Switch FreezeT Identity ()

op'renderAlpha :: (KnownName xs, Op'Render ∈ xs) => Double -> Getter (Widget xs) (GameM ())
op'renderAlpha d = to $ \w -> w ^. _value (Op'Render d)

op'render :: (Given StyleSheet, KnownName xs, Op'Render ∈ xs) => Getter (Widget xs) (GameM ())
op'render = op'renderAlpha 1.0

op'run :: (Op'Run ∈ xs) => Getter (Widget xs) (GameM (Widget xs))
op'run = _self Op'Run

op'reset :: (Op'Reset arg ∈ xs) => arg -> Getter (Widget xs) (Widget xs)
op'reset = _self' . Op'Reset

op'handleEvent :: Op'HandleEvent ∈ xs => M.Map SDL.Scancode Int -> Getter (Widget xs) (GameM (Widget xs))
op'handleEvent = _self . Op'HandleEvent

op'switch :: Op'Switch ∈ xs => Getter (Widget xs) (FreezeT (Widget xs) Identity ())
op'switch = _Op Op'Switch

--

override :: (Widget old -> Widget new) -> Widget old -> (forall br m. Union new br m ~> (br (Widget new) m `Sum` Union old br m)) -> Widget new
override updater wx fu = Widget $ elim id (bimapT updater id . runWidget wx) . fu where
  elim :: (f ~> r) -> (g ~> r) -> (f `Sum` g ~> r)
  elim f g x = case x of
    InL a -> f a
    InR a -> g a

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

runSwitch :: Widget xs -> Getter (Widget xs) (FreezeT (Widget xs) Identity a) -> (Freeze (Widget xs) a -> r) -> r
runSwitch w op k = k $ runIdentity $ runFreezeT (w ^. op)

runSwitchM :: (k ∈ xs, Monad m) => Widget xs -> k FreezeT m a -> (Freeze (Widget xs) a -> m r) -> m r
runSwitchM w op k = runFreezeT (w `call` op) >>= k

op'isFreeze :: Widget xs -> Getter (Widget xs) (FreezeT (Widget xs) Identity a) -> Bool 
op'isFreeze w op = runSwitch w op isFreeze

-- config

type WConfigR a b = Record
  [ "wix" >: WidgetId
  , "required" >: a
  , "optional" >: b
  ]

type WConfig k = WConfigR (Record (Required k)) (Record (Optional k))

class Conf k where
  type Required k :: [Assoc Symbol *]
  type Optional k :: [Assoc Symbol *]
  def :: Record (Optional k)

conf :: WidgetId -> Record (Required k) -> Record (Optional k) -> WConfig k
conf wix req opt = #wix @= wix <: #required @= req <: #optional @= opt <: emptyRecord

giveWid :: (KnownSymbol k) => Proxy k -> WConfig k -> WConfig k
giveWid w wcfg = wcfg & #wix %~ (</> WId (symbolVal w))

getLocation :: (Given StyleSheet) => WidgetId -> SDL.V2 Int
getLocation wix = given ^. wlocation wix

mkCfg :: WidgetId -> a -> b -> WConfigR a b
mkCfg wid req opt
  = #wix @= wid
  <: #required @= req
  <: #optional @= opt
  <: emptyRecord

data ViewWConfig a b = ViewWConfig WidgetId a b

viewWConfig :: WConfigR a b -> ViewWConfig a b
viewWConfig cfg = ViewWConfig (cfg ^. #wix) (cfg ^. #required) (cfg ^. #optional)

wconf :: (KnownSymbol k) => Proxy k -> WConfig k -> ViewWConfig (Record (Required k)) (Record (Optional k))
wconf p = viewWConfig . giveWid p

