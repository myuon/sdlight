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
module SDLight.Widgets.Core where

import Control.Monad.State.Strict
import Control.Object
import Data.Proxy
import Data.Functor.Sum

statefulM :: Monad m => (forall a. t a -> StateT s m a) -> m s -> Object t m
statefulM h = go where
  go s = Object $ \f -> s >>= runStateT (h f) >>= \(a, s') -> s' `seq` return (a, go (return s'))

infix 1 @@~
(@@~) :: Monad m => m s -> (forall a. t a -> StateT s m a) -> Object t m
m @@~ f = statefulM f m

--

data Union (r :: [* -> *]) v where
  UNow  :: t v -> Union (t : r) v
  UNext :: Union (t : r) v -> Union (any : t : r) v

class Member (ts :: [* -> *]) (x :: * -> *) where
  liftU :: (forall v. x v -> r v) -> (forall v. Union ts v -> r v) -> (forall v. Union ts v -> r v)
  
instance Member (t : ts) t where
  liftU f g (UNow tv) = f tv
  
instance Member ts t => Member (any : ts) t where
  liftU f g (UNow av) = g (UNow av)
  liftU f g (UNext t) = liftU f (g . UNext) t

class Include (ts :: [k]) (xs :: [k])
instance Include ts '[]
instance (Include ts xs, Member ts x) => Include ts (x : xs)

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

type Singleton x = Union '[x]

override :: Member xs x => (forall v. x v -> m v) -> Object (Union xs) (m `Sum` (Union xs))
override f = _

{-
type family Pred (n :: Nat) :: Nat where
  Pred Z = Z
  Pred (S n) = n

proxyPred :: Proxy n -> Proxy (Pred n)
proxyPred _ = Proxy

proxySucc :: Proxy n -> Proxy (S n)
proxySucc _ = Proxy

type family Find (t :: * -> *) r :: Nat where
  Find t (t : _) = Z
  Find t (_ : r) = S (Find t r)

class Member' t r (n :: Nat) where
  inj' :: Proxy n -> t v -> Union r v
  prj' :: Proxy n -> Union r v -> t v

instance Member' t (t : r) Z where
  inj' _ = UNow
  prj' _ (UNow t) = t

instance Member' t (r : r') n => Member' t (any : r : r') (S n) where
  inj' p t = UNext (inj' (proxyPred p) t)
  prj' p (UNext t) = prj' (proxyPred p) t

class Member t r where
  inj :: t v -> Union r v
  prj :: Union r v -> t v
-}


