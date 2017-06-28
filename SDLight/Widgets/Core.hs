{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

data Union (r :: [(* -> *) -> * -> *]) m v where
  UNow  :: t m v -> Union (t : r) m v
  UNext :: Union r m v -> Union (any : r) m v

class Member ts x where
  inj :: x m ~> Union ts m

instance {-# OVERLAPPING #-} Member (t : ts) t where
  inj = UNow
  
instance {-# OVERLAPPABLE #-} Member ts t => Member (any : ts) t where
  inj xv = UNext (inj xv)

type (∈) x xs = Member xs x

class CaseOf r t rs | r t -> rs where
  caseOf :: Union r m v -> Either (Union rs m v) (t m v)

instance CaseOf (r : rs) r rs where
  caseOf (UNow tv) = Right tv
  caseOf (UNext n) = Left n

infixr 2 @>
(@>) :: (t m ~> r) -> (Union ts m ~> r) -> Union (t : ts) m ~> r
(@>) f g u = either g f $ caseOf u

emptyUnion :: Union '[] m v -> a
emptyUnion = \case

newtype Op m r (op :: (* -> *) -> * -> *) = Op { runOp :: op m r }
newtype Widget ops = Widget { runWidget :: forall m. Monad m => Union ops m ~> EitherT (Widget ops) m }

call :: (k ∈ xs, Monad m) => Widget xs -> (forall v. k m v -> m (Either (Widget xs) v))
call w op = runEitherT $ runWidget w $ inj op

infixl 4 @!
(@!) :: (k ∈ xs, Monad m) => Widget xs -> (k m ~> m)
w @! op = (\(Right v) -> v) <$> w `call` op

infixl 4 @@!
(@@!) :: (k ∈ xs) => Widget xs -> (k Identity v -> v)
w @@! op = runIdentity $ w @! op

infixl 4 @.
(@.) :: (k ∈ xs, Monad m) => Widget xs -> k m Void -> m (Widget xs)
w @. op = w `call` op >>= \case
  Left w' -> return w'
  Right v -> absurd v

infixl 4 @@.
(@@.) :: (k ∈ xs) => Widget xs -> k Identity Void -> Widget xs
w @@. op = runIdentity $ w @. op

infixr 4 @%~
(@%~) :: (k ∈ xs) => Lens' s (Widget xs) -> k Identity Void -> s -> s
w @%~ k = w %~ (@@. k)

continue :: (model -> Widget xs) -> model -> EitherT (Widget xs) Identity a
continue go = EitherT . Identity . Left . go

continueM :: Functor m => (model -> Widget xs) -> m model -> EitherT (Widget xs) m a
continueM go v = EitherT $ Left . go <$> v

finish :: Monad m => model -> EitherT (Widget xs) m model
finish = right

finishM :: Functor m => m model -> EitherT (Widget xs) m model
finishM = EitherT . (Right <$>)

--

data Seq args where
  SNil :: Seq '[]
  SCons :: x -> Seq xs -> Seq (x : xs)

pattern (:.) x y = SCons x y

type NoValue = Void

data Op'Render m r where
  Op'Render :: SDL.V2 Int -> Op'Render GameM ()

data Op'Run m r where
  Op'Run :: Op'Run GameM Void

data Op'Reset args m r where
  Op'Reset :: Seq args -> Op'Reset args Identity Void

data Op'HandleEvent m r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent GameM Void

data Op'IsFinished m r where
  Op'IsFinished :: Op'IsFinished Identity Bool

override :: (Widget old -> Widget new) -> Widget old -> (forall m. Union new m ~> (EitherT (Widget new) m `Sum` Union old m)) -> Widget new
override updater wx f = Widget $ elim id (bimapEitherT updater id . runWidget wx) . f where
  elim :: (f ~> r) -> (g ~> r) -> (f `Sum` g ~> r)
  elim f g x = case x of
    InL a -> f a
    InR a -> g a

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

