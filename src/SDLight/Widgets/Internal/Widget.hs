{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-|
Definition of Widget
-}
module SDLight.Widgets.Internal.Widget where

import Control.Lens
import Control.Monad.Trans
import Data.Extensible

-- | Polymorphic function type
type (~>) f g = forall x. f x -> g x

-- | Operator type
newtype Op br m r (op :: (* -> (* -> *) -> * -> *) -> (* -> *) -> * -> *) = Op { runOp :: op br m r }

-- | @Union ops br m v@ represents operator list of @ops@ with each operator has type of @'Op' br m v@
newtype Union ops br m v = Union { getUnion :: Op br m v :| ops }

-- | Widget Type
newtype Widget ops = Widget { runWidget :: forall br m. (Functor m, TransBifunctor br m) => Union ops br m ~> br (Widget ops) m }

-- |
-- @f@ is like a bifunctor with base monad @m@
class TransBifunctor f (m :: * -> *) where
  bimapT :: (a -> b) -> (c -> d) -> f a m c -> f b m d

  firstT :: (a -> b) -> f a m c -> f b m c
  firstT f = bimapT f id

  secondT :: (c -> d) -> f a m c -> f a m d
  secondT g = bimapT id g

-- | Call a method of a widget
call :: (k ∈ xs, TransBifunctor br m, Functor m) => Widget xs -> (k br m ~> br (Widget xs) m)
call w = runWidget w . Union . embed . Op

-- | Turn a operator into Getter
_Op :: (k ∈ xs, TransBifunctor br m, Functor m) => k br m a -> Getter (Widget xs) (br (Widget xs) m a)
_Op opr = to (\w -> call w opr)

infixr 2 @>

-- | Cons operator of Union type
(@>) :: (t br m ~> r) -> (Union ts br m ~> r) -> (Union (t : ts) br m ~> r)
(@>) f g = ((f . runOp) <:| (g . Union)) . getUnion

-- | Exhausts empty union
emptyUnion :: Union '[] br m v -> a
emptyUnion = \case

--

-- | Represents that an operator returns @w@, usually widget itself
newtype Self w m a = Self { runSelf :: m w }

-- | Represents that an operator returns @a@, usually a value
newtype Value w m a = Value { getValue :: m a }

instance MonadTrans (Value w) where
  lift = Value

instance Functor m => TransBifunctor Self m where
  bimapT f _ = Self . fmap f . runSelf

instance Functor m => TransBifunctor Value m where
  bimapT _ g = Value . fmap g . getValue

-- | @br@ can continue with the current widget state
class NodeW br where
  continue :: Monad m => Widget xs -> br (Widget xs) m a
  continueM :: Functor m => m (Widget xs) -> br (Widget xs) m a

  _self :: (k ∈ xs, TransBifunctor br m, Functor m) => k br m () -> Getter (Widget xs) (m (Widget xs))

-- | @br@ can finish with given value
class LeafW br where
  finish :: Monad m => model -> br (Widget xs) m model
  finishM :: Functor m => m model -> br (Widget xs) m model

  _value :: (k ∈ xs, Functor m) => k br m a -> Getter (Widget xs) (m a)

instance NodeW Self where
  continue = Self . return
  continueM = Self

  _self opr = to $ \w -> runSelf $ w `call` opr

instance LeafW Value where
  finish = Value . return
  finishM = Value

  _value opr = to $ \w -> getValue $ w `call` opr

-- | '_self' with Identity monad
_self' :: (k ∈ xs, TransBifunctor br Identity, NodeW br) => k br Identity () -> Getter (Widget xs) (Widget xs)
_self' opr = _self opr . to runIdentity

-- | '_value'' with Identity monad
_value' :: (k ∈ xs, LeafW br) => k br Identity a -> Getter (Widget xs) a
_value' opr = _value opr . to runIdentity


