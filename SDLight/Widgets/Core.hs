{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module SDLight.Widgets.Core where
  
import qualified SDL as SDL
import Control.Arrow (first, second)
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Functor.Sum
import Data.Void
import SDLight.Types
import GHC.Exts

type (~>) f g = forall x. f x -> g x

data Union (r :: [(* -> (* -> *) -> * -> *) -> (* -> *) -> * -> *]) br m v where
  UNow  :: t br m v -> Union (t : r) br m v
  UNext :: Union r br m v -> Union (any : r) br m v

class Member ts x where
  inj :: x br m ~> Union ts br m

instance {-# OVERLAPPING #-} Member (t : ts) t where
  inj = UNow
  
instance {-# OVERLAPPABLE #-} Member ts t => Member (any : ts) t where
  inj xv = UNext (inj xv)

type (∈) x xs = Member xs x

class CaseOf r t rs | r t -> rs where
  caseOf :: Union r br m v -> Either (Union rs br m v) (t br m v)

instance CaseOf (r : rs) r rs where
  caseOf (UNow tv) = Right tv
  caseOf (UNext n) = Left n

infixr 2 @>
(@>) :: (t br m ~> r) -> (Union ts br m ~> r) -> Union (t : ts) br m ~> r
(@>) f g u = either g f $ caseOf u

emptyUnion :: Union '[] br m v -> a
emptyUnion = \case

class TransBifunctor f m where
  bimapT :: (a -> b) -> (c -> d) -> f a m c -> f b m d

  firstT :: (a -> b) -> f a m c -> f b m c
  firstT f = bimapT f id

  secondT :: (c -> d) -> f a m c -> f a m d
  secondT g = bimapT id g

newtype Op m r (op :: (* -> *) -> * -> *) = Op { runOp :: op m r }
newtype Widget ops = Widget { runWidget :: forall br m. (Functor m, TransBifunctor br m) => Union ops br m ~> br (Widget ops) m }

newtype Self w m a = Self { runSelf :: m w }
newtype Value w m a = Value { getValue :: m a }

instance MonadTrans (Value w) where
  lift = Value

instance Functor m => TransBifunctor EitherT m where
  bimapT = bimapEitherT

instance Functor m => TransBifunctor Self m where
  bimapT f g = Self . fmap f . runSelf

instance Functor m => TransBifunctor Value m where
  bimapT f g = Value . fmap g . getValue

call :: (k ∈ xs, TransBifunctor br m, Functor m) => Widget xs -> (k br m ~> br (Widget xs) m)
call w op = runWidget w $ inj op

class NodeW br where
  continue :: (model -> Widget xs) -> model -> br (Widget xs) Identity a
  continueM :: Functor m => (model -> Widget xs) -> m model -> br (Widget xs) m a

  infixl 4 @.
  (@.) :: (k ∈ xs, TransBifunctor br m, Functor m) => Widget xs -> k br m Void -> m (Widget xs)

class LeafW br where
  finish :: model -> br (Widget xs) Identity model
  finishM :: Functor m => m model -> br (Widget xs) m model

  infixl 4 @!
  (@!) :: (k ∈ xs, Functor m) => Widget xs -> (k br m ~> m)

infixl 4 @@.
(@@.) :: (k ∈ xs, NodeW br, TransBifunctor br Identity) => Widget xs -> k br Identity Void -> Widget xs
w @@. op = runIdentity $ w @. op

infixl 4 @@!
(@@!) :: (LeafW br, k ∈ xs) => Widget xs -> (k br Identity v -> v)
w @@! op = runIdentity $ w @! op

instance NodeW Self where
  continue go = Self . Identity . go
  continueM go = Self . (go <$>)

  w @. op = runSelf $ w `call` op

instance LeafW Value where
  finish = Value . Identity
  finishM = Value

  w @! op = getValue $ w `call` op

instance NodeW EitherT where
  continue go = left . go
  continueM go v = EitherT $ Left . go <$> v

  w @. op = runEitherT (w `call` op) <&> \case
    Left w' -> w'
    Right v -> absurd v

instance LeafW EitherT where
  finish = right
  finishM = EitherT . (Right <$>)

  w @! op = (\(Right v) -> v) <$> runEitherT (w `call` op)

infixr 4 @%~
(@%~) :: (k ∈ xs, NodeW br, TransBifunctor br Identity) => Lens' s (Widget xs) -> k br Identity Void -> s -> s
w @%~ k = w %~ (@@. k)

infixr 4 <@%~
(<@%~) :: (k ∈ xs, Functor m, TransBifunctor br m, NodeW br) => Lens' s (Widget xs) -> k br m Void -> s -> m s
(<@%~) w k s = (s^.w @. k) <&> \w' -> s & w .~ w'

--

data Seq args where
  SNil :: Seq '[]
  SCons :: x -> Seq xs -> Seq (x : xs)

pattern (:.) x y = SCons x y

type NoValue = Void

data Op'Render br m r where
  Op'Render :: SDL.V2 Int -> Op'Render Value GameM ()

data Op'Run br m r where
  Op'Run :: Op'Run Self GameM Void

data Op'Reset args br m r where
  Op'Reset :: Seq args -> Op'Reset args Self Identity Void

data Op'HandleEvent br m r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent Self GameM Void

data Op'IsFinished br m r where
  Op'IsFinished :: Op'IsFinished Value Identity Bool

override :: (Widget old -> Widget new) -> Widget old -> (forall br m. Union new br m ~> (br (Widget new) m `Sum` Union old br m)) -> Widget new
override updater wx f = Widget $ elim id (bimapT updater id . runWidget wx) . f where
    elim :: (f ~> r) -> (g ~> r) -> (f `Sum` g ~> r)
    elim f g x = case x of
      InL a -> f a
      InR a -> g a

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

onFinish :: (Op'IsFinished ∈ xs, k ∈ xs, TransBifunctor br m, NodeW br, Monad m) => Lens' s (Widget xs) -> k br m Void -> s -> (s -> m s) -> m s
onFinish lens op s cb = do
  s' <- s & lens <@%~ op
  if s'^.lens @@! Op'IsFinished
    then cb s'
    else return s'

