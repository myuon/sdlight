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

  , Op'New(..)
  , Op'Render(..)
  , Op'Run(..)
  , Op'Reset(..)
  , Op'HandleEvent(..)

  , This(..)
  ) where

import qualified SDL as SDL
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Proxy
import qualified Data.Map as M
import SDLight.Types

type (~>) f g = forall x. f x -> g x
type (~~>) f g = forall x y. f x y -> g x y

data Union (r :: [(* -> *) -> * -> *]) m v where
  UNow  :: t m v -> Union (t : r) m v
  UNext :: Union r m v -> Union (any : r) m v

class Member ts x where
  liftU :: (x m ~> r) -> (Union ts m ~> r) -> (Union ts m ~> r)
  inj :: x m ~> Union ts m

instance {-# OVERLAPPING #-} Member (t : ts) t where
  liftU f g (UNow tv) = f tv
  inj = UNow
  
instance {-# OVERLAPPABLE #-} Member ts t => Member (any : ts) t where
  liftU f g (UNow av) = g (UNow av)
  liftU f g (UNext t) = liftU f (g . UNext) t

  inj xv = UNext (inj xv)

class PolyMember (x :: k) (xs :: [k])
instance {-# OVERLAPPING #-} PolyMember x (x : xs)
instance {-# OVERLAPPABLE #-} PolyMember x xs => PolyMember x (any : xs)

instance Member xs x => PolyMember x xs

type (∈) = PolyMember

class Include (ts :: [k]) (xs :: [k])
instance Include ts '[]
instance (Include ts xs, Member ts x) => Include ts (x : xs)

type (⊆) xs ys = Include ys xs

class CaseOf r t rs | r t -> rs where
  caseOf :: Union r m v -> Either (Union rs m v) (t m v)

instance CaseOf (r : rs) r rs where
  caseOf (UNow tv) = Right tv
  caseOf (UNext n) = Left n

class ExtendU xs ys where
  extendU :: Union xs m ~> Union ys m

instance ExtendU '[] ys where
  extendU = emptyUnion

instance Member ys x => ExtendU (x : xs) ys where
  extendU (UNow xv) = inj xv

infixr 2 @>
(@>) :: (t m ~> r) -> (Union ts m ~> r) -> Union (t : ts) m ~> r
(@>) f g u = either g f $ caseOf u

emptyUnion :: Union '[] m v -> a
emptyUnion = \case

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

infixr 5 :*
type family (:*) k xs where
  k :* '[] = '[]
  k :* (x : xs) = k x : (k :* xs)

infixr 5 :$
type family (:$) (xs :: [* -> (* -> *) -> * -> *]) (a :: *) = r | r -> xs where
  '[] :$ a = '[]
  (x : xs) :$ a = x a : (xs :$ a)

--

newtype Eff ts s m = Eff { runEff :: s -> Union (ts :$ s) m ~> m }

emptyEff :: Eff '[] s m
emptyEff = Eff $ \_ -> emptyUnion

infix 6 @!
(@!) :: Member (xs :$ s) (x s) => Eff xs s m -> (forall v. x s m v -> s -> m v)
(@!) ef method s = runEff ef s (inj method)

data Op'Render s m r where
  Op'Render :: SDL.V2 Int -> Op'Render s GameM ()

data Op'Run s m r where
  Op'Run :: Op'Run s GameM s

class Coproduct xs ys where
  coproduct :: Lens' st s -> Lens' st t -> Eff xs s m -> Eff ys t m -> Eff (xs ++ ys) st m

instance Coproduct (x : xs) ys where
  coproduct slens tlens xs ys = Eff $ \st -> \case
    UNow xv -> xs @! xv $ st^.slens

{-
newtype Eff ts m = Eff { runEff :: Union ts m ~> m }

override :: Member xs x => Eff xs m -> (x m ~> m) -> Eff xs m
override ef f = Eff $ liftU f (runEff ef)

infixr 2 @>>
(@>>) :: (x m ~> m) -> Eff xs m -> Eff (x : xs) m
f @>> ef = Eff $ \u -> either (runEff ef) f $ caseOf u

emptyEff :: Eff '[] m
emptyEff = Eff emptyUnion

infix 6 @!
(@!) :: Member xs x => Eff xs m -> (x m ~> m)
ef @! method = runEff ef (inj method)

-- singleton operators

data Seq (xs :: [*]) where
  SEmpty :: Seq '[]
  SCons :: a -> Seq xs -> Seq (a : xs)

infixr 2 :.
pattern (:.) a b = SCons a b

data Op'New args s m r where
  Op'New :: Seq args -> Op'New args s GameM s

data Op'Render s m r where
  Op'Render :: SDL.V2 Int -> Op'Render s (StateT s GameM) ()

data Op'Run s m r where
  Op'Run :: Op'Run s (StateT s m) ()

data Op'Reset s m r where
  Op'Reset :: Op'Reset s ((->) s) s

data Op'HandleEvent s m r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent s (StateT s m) ()

data This = forall a. This a
-}

--


