{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad.Except
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

instance Functor m => TransBifunctor Self m where
  bimapT f g = Self . fmap f . runSelf

instance Functor m => TransBifunctor Value m where
  bimapT f g = Value . fmap g . getValue

call :: (k ∈ xs, TransBifunctor br m, Functor m) => Widget xs -> (k br m ~> br (Widget xs) m)
call w op = runWidget w $ inj op

class NodeW br where
  continue :: Monad m => Widget xs -> br (Widget xs) m a
  continueM :: Functor m => m (Widget xs) -> br (Widget xs) m a

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
  continue = Self . return
  continueM = Self

  w @. op = runSelf $ w `call` op

instance LeafW Value where
  finish = Value . Identity
  finishM = Value

  w @! op = getValue $ w `call` op

infixr 4 @%~
(@%~) :: (k ∈ xs, NodeW br, TransBifunctor br Identity) => Lens' s (Widget xs) -> k br Identity Void -> s -> s
w @%~ k = w %~ (@@. k)

infixr 4 <@%~
(<@%~) :: (k ∈ xs, Functor m, TransBifunctor br m, NodeW br) => Lens' s (Widget xs) -> k br m Void -> s -> m s
(<@%~) w k s = (s^.w @. k) <&> \w' -> s & w .~ w'

--

data Freeze w a = Freeze w a | Keep w

makePrisms ''Freeze

refreeze :: (w -> z) -> Freeze w a -> Freeze z b
refreeze f (Freeze w a) = Keep (f w)
refreeze f (Keep w) = Keep (f w)

unfreeze :: (w -> a -> r) -> (w -> r) -> Freeze w a -> r
unfreeze fr ke (Freeze w a) = fr w a
unfreeze fr ke (Keep w) = ke w

_Frozen :: Lens' (Freeze w a) w
_Frozen = lens get set where
  get (Freeze w _) = w
  get (Keep w) = w

  set (Freeze _ a) w = Freeze w a
  set (Keep _) w = Keep w

data Op'Render br m r where
  Op'Render :: SDL.V2 Int -> Op'Render Value GameM ()

data Op'Run br m r where
  Op'Run :: Op'Run Self GameM a

data Op'Reset arg br m r where
  Op'Reset :: arg -> Op'Reset arg Self Identity a

data Op'HandleEvent br m r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent Self GameM a


data Op'Switch br m r where
  Op'Switch :: Op'Switch FreezeT GameM ()

newtype FreezeT w m a = FreezeT { runFreezeT :: m (Freeze w a) }

instance Functor m => TransBifunctor FreezeT m where
  bimapT f g = FreezeT . fmap (unfreeze (\a c -> Freeze (f a) (g c)) (Keep . f)) . runFreezeT

instance NodeW FreezeT where
  continue = FreezeT . return . Keep
  continueM = FreezeT . fmap Keep

  w @. op = fmap (^._Frozen) $ runFreezeT $ w `call` op

freeze :: Monad m => Widget xs -> a -> FreezeT (Widget xs) m a
freeze w a = FreezeT $ return $ Freeze w a

freeze' :: Monad m => Widget xs -> FreezeT (Widget xs) m ()
freeze' w = freeze w ()

freezeM :: Monad m => m (Widget xs) -> a -> FreezeT (Widget xs) m a
freezeM mw a = FreezeT $ fmap (\w -> Freeze w a) mw

freezeM' :: Monad m => m (Widget xs) -> FreezeT (Widget xs) m ()
freezeM' w = freezeM w ()

override :: (Widget old -> Widget new) -> Widget old -> (forall br m. Union new br m ~> (br (Widget new) m `Sum` Union old br m)) -> Widget new
override updater wx f = Widget $ elim id (bimapT updater id . runWidget wx) . f where
  elim :: (f ~> r) -> (g ~> r) -> (f `Sum` g ~> r)
  elim f g x = case x of
    InL a -> f a
    InR a -> g a

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

onFreeze :: (k ∈ xs, TransBifunctor FreezeT m, Monad m) => Lens' s (Widget xs) -> k FreezeT m a -> s -> (a -> Widget xs -> m s) -> m s
onFreeze lens op s cb = do
  fw <- runFreezeT $ (s^.lens) `call` op
  case fw of
    Freeze w a -> cb a w
    Keep w -> return $ s & lens .~ w

onFreeze' :: (k ∈ xs, TransBifunctor FreezeT m, Monad m) => Lens' s (Widget xs) -> k FreezeT m a -> s -> (Widget xs -> m s) -> m s
onFreeze' lens op s cb = onFreeze lens op s (\_ -> cb)

onFinish :: (k ∈ xs, TransBifunctor FreezeT m, Monad m) => Lens' s (Widget xs) -> k FreezeT m a -> s -> (a -> s -> m s) -> m s
onFinish lens op s cb = onFreeze lens op s (\a w -> cb a (s & lens .~ w))

runSwitch :: (k ∈ xs, Monad m) => Widget xs -> k FreezeT m a -> (Freeze (Widget xs) a -> m r) -> m r
runSwitch w op k = runFreezeT (w `call` op) >>= k


