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
import Data.Void
import SDLight.Types

type (~>) f g = forall x. f x -> g x
type (~~>) f g = forall m. Monad m => f m ~> g m

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

type family (:*) (k :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *)) xs = result | result -> xs where
  k :* '[] = '[]
  k :* (x : xs) = k x : (k :* xs)

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

--

newtype Widget ops = Widget { runWidget :: forall m. Monad m => Union ops m ~> EitherT (Widget ops) m }

tailW :: Widget (x : xs) -> Widget xs
tailW wx = Widget $ \u -> bimapEitherT tailW id $ runWidget wx (UNext u)

headW :: Monad m => Widget (x : xs) -> x m ~> EitherT (Widget (x : xs)) m
headW wx xv = runWidget wx (UNow xv)

infixr 2 @>>
(@>>) :: (forall m. Monad m => x m ~> m) -> Widget xs -> Widget (x : xs)
f @>> wx = Widget $ \case
  UNow xv -> EitherT $ Right <$> f xv
  UNext t -> bimapEitherT (f @>>) id $ runWidget wx t

infixr 2 @?>
(@?>) :: (x ~~> EitherT (Widget (x : xs))) -> Widget xs -> Widget (x : xs)
f @?> wx = Widget $ \case
  UNow xv -> f xv
  UNext t -> bimapEitherT (f @?>) id $ runWidget wx t

-- 

class Wrap xs k where
  wrap :: Union xs ~~> Union (k :* xs)
  unwrap :: Union (k :* xs) ~~> Union xs

instance Wrap '[] Op'Lift where
  wrap = id
  unwrap = id

instance Wrap xs Op'Lift => Wrap (x : xs) Op'Lift where
  wrap = \case
    UNow xv -> UNow $ Op'Lift xv
    UNext t -> UNext $ wrap @xs @Op'Lift t
  unwrap = \case
    UNow (Op'Lift xv) -> UNow xv
    UNext t -> UNext $ unwrap @xs @Op'Lift t

wlift :: (Wrap xs Op'Lift) => Widget xs -> Widget (Op'Lift :* xs)
wlift wx = Widget $ \u -> bimapEitherT wlift id $ runWidget wx (unwrap @_ @Op'Lift u)

wunlift :: (Wrap xs Op'Lift) => Widget (Op'Lift :* xs) -> Widget xs
wunlift wx = Widget $ \u -> bimapEitherT wunlift id $ runWidget wx (wrap @_ @Op'Lift u)

type (:<<:) xs ys = ys ++ (Op'Lift :* xs)

class Extend xs ys where
  extend :: Widget xs -> Widget ys -> Widget (xs :<<: ys)

instance Wrap xs Op'Lift => Extend xs '[] where
  extend wx _ = wlift wx

instance (Wrap xs Op'Lift, Extend xs ys) => Extend xs (y : ys) where
  extend wx wy = Widget $ \case
    UNow yv -> bimapEitherT (extend wx) id $ runWidget wy (UNow yv)
    UNext t ->
      let go :: (y ~~> EitherT (Widget (y : ys))) -> Widget (ys ++ (Op'Lift :* xs)) -> Widget (y : ys ++ (Op'Lift :* xs))
          go ym ysxs = bimapEitherT (\yys -> go (headW yys) ysxs) id . ym @?> ysxs in
      bimapEitherT (go (headW wy)) id $ runWidget (wx `extend` tailW wy) t

override :: x ∈ xs => Widget xs -> (x ~~> EitherT (Widget xs)) -> Widget xs
override widget f = Widget $ liftU f (runWidget widget)

--

call :: (k ∈ xs, Monad m) => Widget xs -> (forall v. k m v -> m (Either (Widget xs) v))
call w op = runEitherT $ runWidget w (inj op)

infixl 4 @!
(@!) :: (k ∈ xs, Monad m) => Widget xs -> (forall v. k m v -> m (Either (Widget xs) v))
w @! op = runEitherT $ runWidget w (inj op)

infixl 4 @@!
(@@!) :: (k ∈ xs) => Widget xs -> (forall v. k Identity v -> Either (Widget xs) v)
w @@! op = runIdentity $ w @! op

infixl 4 @!?
(@!?) :: (k ∈ xs, Monad m) => Widget xs -> (k m ~> m)
w @!? op = (\(Right v) -> v) <$> (runEitherT $ runWidget w (inj op))

infixl 4 @@!?
(@@!?) :: (k ∈ xs) => Widget xs -> (k Identity v -> v)
w @@!? op = runIdentity $ w @!? op

infixl 4 @!!
(@!!) :: (k ∈ xs, Monad m) => m (Widget xs) -> (forall v. k m v -> m (Either (Widget xs) v))
mw @!! op = mw >>= \w -> w @! op

infixl 4 @.
(@.) :: (k ∈ xs, Monad m) => Widget xs -> k m Void -> m (Widget xs)
w @. op = w @! op >>= \case
  Left w' -> return w'
  Right v -> absurd v

infixl 4 @@.
(@@.) :: (k ∈ xs) => Widget xs -> k Identity Void -> Widget xs
w @@. op = runIdentity $ w @. op

infixl 4 @..
(@..) :: (k ∈ xs, Monad m) => m (Widget xs) -> k m Void -> m (Widget xs)
mw @.. op = mw >>= \w -> w @. op

infixr 4 @@~
(@@~) :: (k ∈ xs) => Lens' s (Widget xs) -> k Identity Void -> s -> s
w @@~ k = w %~ (@@. k)

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

data Op'Lift k (m :: * -> *) a where
  Op'Lift :: k m a -> Op'Lift k m a

type Lifting xs = Wrap xs Op'Lift
type Lifted xs = Op'Lift :* xs

--

continue :: (model -> Widget xs) -> model -> EitherT (Widget xs) Identity a
continue go = EitherT . Identity . Left . go

continueM :: Functor m => (model -> Widget xs) -> m model -> EitherT (Widget xs) m a
continueM go v = EitherT $ Left . go <$> v

finish :: Monad m => model -> EitherT (Widget xs) m model
finish = right

finishM :: Functor m => m model -> EitherT (Widget xs) m model
finishM = EitherT . (Right <$>)

