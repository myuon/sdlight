{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module SDLight.Widgets.Core
  ( type (~>)
  , type (~~>)
  , type (∈)
  , type (⊆)
  , Union(..)
  , Member
  , type (++)
  , type (:*)
  , type (:$)
  
  , Eff(..)
  , Eff'
  , (@>>)
  , emptyEff
  , (@!)
  , Seq(..)
  , pattern (:.)
  , Lifts
  , oplift

  , Op'New(..)
  , Op'Render(..)
  , Op'Run(..)
  , Op'Reset(..)
  , Op'HandleEvent(..)
  , Op'Lift(..)
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad.State.Strict
import Data.Proxy
import qualified Data.Map as M

type (~>) f g = forall x. f x -> g x
type (~~>) f g = forall x y. f x y -> g x y

data Union (r :: [* -> *]) v where
  UNow  :: t v -> Union (t : r) v
  UNext :: Union r v -> Union (any : r) v

class Member (ts :: [* -> *]) (x :: * -> *) where
  liftU :: (x ~> r) -> (Union ts ~> r) -> (Union ts ~> r)
  inj :: x ~> Union ts
  
instance Member (t : ts) t where
  liftU f g (UNow tv) = f tv
  inj = UNow
  
instance Member ts t => Member (any : ts) t where
  liftU f g (UNow av) = g (UNow av)
  liftU f g (UNext t) = liftU f (g . UNext) t

  inj xv = UNext (inj xv)

type (∈) x xs = Member xs x

class Include (ts :: [k]) (xs :: [k])
instance Include ts '[]
instance (Include ts xs, Member ts x) => Include ts (x : xs)

type (⊆) xs ys = Include ys xs

class CaseOf r t rs | r t -> rs where
  caseOf :: Union r v -> Either (Union rs v) (t v)

instance CaseOf (r : rs) r rs where
  caseOf (UNow tv) = Right tv
  caseOf (UNext n) = Left n

infixr 2 @>
(@>) :: (t v -> r) -> (Union ts v -> r) -> Union (t : ts) v -> r
(@>) f g u = either g f $ caseOf u

emptyUnion :: Union '[] v -> a
emptyUnion = \case

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

infixr 5 :*
type family (:*) k xs where
  k :* '[] = '[]
  k :* (x : xs) = k x : (k :* xs)

infixr 5 :$
type family (:$) xs a where
  '[] :$ a = '[]
  (x : xs) :$ a = x a : (xs :$ a)

type family All k xs where
  All k '[] = ()
  All k (x : xs) = (k x , All k xs)

--

newtype Eff (ts :: [* -> *]) m = Eff { runEff :: Union ts ~> m }
type Eff' (ts :: [* -> * -> *]) this m = Eff (ts :$ this) m

override :: Member xs x => Eff xs m -> (x ~> m) -> Eff xs m
override ef f = Eff $ liftU f (runEff ef)

infixr 2 @>>
(@>>) :: (x ~> m) -> Eff xs m -> Eff (x : xs) m
f @>> ef = Eff $ \u -> either (runEff ef) f $ caseOf u

emptyEff :: Eff '[] m
emptyEff = Eff emptyUnion

infix 6 @!
(@!) :: Member xs x => Eff xs m -> (forall v. x v -> m v)
ef @! method = runEff ef (inj method)

-- singleton operators

data Seq (xs :: [*]) where
  SEmpty :: Seq '[]
  SCons :: a -> Seq xs -> Seq (a : xs)

infixr 2 :.
pattern (:.) a b = SCons a b

data Op'New args this r where
  Op'New :: Seq args -> Op'New args this this

data Op'Render args this r where
  Op'Render :: Seq args -> this -> Op'Render args this ()

data Op'Run args this r where
  Op'Run :: Seq args -> this -> Op'Run args this this

data Op'Reset args this r where
  Op'Reset :: Seq args -> this -> Op'Reset args this this

data Op'HandleEvent args this r where
  Op'HandleEvent :: Seq args -> M.Map SDL.Scancode Int -> this -> Op'HandleEvent args this this

--

data Op'Lift op r where
  Op'Lift :: op r -> Op'Lift op r

class Lifts ts where
  opliftU :: Union (Op'Lift :* ts) ~> Union ts

instance Lifts '[] where
  opliftU = id

instance Lifts xs => Lifts (x : xs) where
  opliftU (UNow (Op'Lift x)) = UNow x
  opliftU (UNext t) = UNext (opliftU t)

oplift :: Lifts ts => Eff ts m -> Eff (Op'Lift :* ts) m
oplift ef = Eff $ \u -> runEff ef $ opliftU u

