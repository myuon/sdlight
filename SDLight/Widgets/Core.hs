{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
module SDLight.Widgets.Core where
  
import qualified SDL as SDL
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.Functor.Sum
import Data.Void
import SDLight.Types

type (~>) f g = forall x. f x -> g x

infixr 4 ^%~
(^%~) :: Lens' s a -> (Getter a a) -> s -> s
s ^%~ f = s %~ (^. f)

infixr 4 ^%%~
(^%%~) :: Functor m => Lens' s a -> (Getter a (m a)) -> s -> m s
s ^%%~ f = s %%~ (^. f)

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

class TransBifunctor f (m :: * -> *) where
  bimapT :: (a -> b) -> (c -> d) -> f a m c -> f b m d

  firstT :: (a -> b) -> f a m c -> f b m c
  firstT f = bimapT f id

  secondT :: (c -> d) -> f a m c -> f a m d
  secondT g = bimapT id g

newtype Op m r (op :: (* -> *) -> * -> *) = Op { runOp :: op m r }
newtype Widget ops = Widget { runWidget :: forall br m. (Functor m, TransBifunctor br m) => Union ops br m ~> br (Widget ops) m }

call :: (k ∈ xs, TransBifunctor br m, Functor m) => Widget xs -> (k br m ~> br (Widget xs) m)
call w = runWidget w . inj

_Op :: (k ∈ xs, TransBifunctor br m, Functor m) => k br m a -> Getter (Widget xs) (br (Widget xs) m a)
_Op opr = to (\w -> call w opr)

newtype Self w m a = Self { runSelf :: m w }
newtype Value w m a = Value { getValue :: m a }

instance MonadTrans (Value w) where
  lift = Value

instance Functor m => TransBifunctor Self m where
  bimapT f _ = Self . fmap f . runSelf

instance Functor m => TransBifunctor Value m where
  bimapT _ g = Value . fmap g . getValue

class NodeW br where
  continue :: Monad m => Widget xs -> br (Widget xs) m a
  continueM :: Functor m => m (Widget xs) -> br (Widget xs) m a

  _self :: (k ∈ xs, TransBifunctor br m, Functor m) => k br m Void -> Getter (Widget xs) (m (Widget xs))

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

_self' :: (k ∈ xs, TransBifunctor br Identity, NodeW br) => k br Identity Void -> Getter (Widget xs) (Widget xs)
_self' opr = _self opr . to runIdentity

_value' :: (k ∈ xs, LeafW br) => k br Identity a -> Getter (Widget xs) a
_value' opr = _value opr . to runIdentity

--

data Freeze w a = Freeze w a | Keep w

makePrisms ''Freeze

refreeze :: (w -> z) -> Freeze w a -> Freeze z b
refreeze f (Freeze w _) = Keep (f w)
refreeze f (Keep w) = Keep (f w)

unfreeze :: (w -> a -> r) -> (w -> r) -> Freeze w a -> r
unfreeze fr _ (Freeze w a) = fr w a
unfreeze _ ke (Keep w) = ke w

isFreeze :: Freeze w a -> Bool
isFreeze = unfreeze (\_ _ -> True) (\_ -> False)

_Frozen :: Lens' (Freeze w a) w
_Frozen = lens lget lset where
  lget (Freeze w _) = w
  lget (Keep w) = w

  lset (Freeze _ a) w = Freeze w a
  lset (Keep _) w = Keep w

data Op'Render br m r where
  Op'Render :: Double -> SDL.V2 Int -> Op'Render Value GameM ()

data Op'Run br m r where
  Op'Run :: Op'Run Self GameM a

data Op'Reset arg br m r where
  Op'Reset :: arg -> Op'Reset arg Self Identity a

data Op'HandleEvent br m r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent Self GameM a


data Op'Switch br m r where
  Op'Switch :: Op'Switch FreezeT Identity ()

op'renderAlpha :: Op'Render ∈ xs => Double -> SDL.V2 Int -> Getter (Widget xs) (GameM ())
op'renderAlpha d v = to $ \w -> w ^. _value (Op'Render d v)

op'render :: Op'Render ∈ xs => SDL.V2 Int -> Getter (Widget xs) (GameM ())
op'render = op'renderAlpha 1.0

op'run :: Op'Run ∈ xs => Getter (Widget xs) (GameM (Widget xs))
op'run = _self Op'Run

op'reset :: Op'Reset arg ∈ xs => arg -> Getter (Widget xs) (Widget xs)
op'reset = _self' . Op'Reset

op'handleEvent :: Op'HandleEvent ∈ xs => M.Map SDL.Scancode Int -> Getter (Widget xs) (GameM (Widget xs))
op'handleEvent = _self . Op'HandleEvent

newtype FreezeT w m a = FreezeT { runFreezeT :: m (Freeze w a) }

instance Functor m => TransBifunctor FreezeT m where
  bimapT f g = FreezeT . fmap (unfreeze (\a c -> Freeze (f a) (g c)) (Keep . f)) . runFreezeT

instance NodeW FreezeT where
  continue = FreezeT . return . Keep
  continueM = FreezeT . fmap Keep

  _self opr = to $ \w -> fmap (^._Frozen) $ runFreezeT $ w `call` opr

op'switch :: Op'Switch ∈ xs => Getter (Widget xs) (FreezeT (Widget xs) Identity ())
op'switch = _Op Op'Switch

--

freeze :: Monad m => Widget xs -> a -> FreezeT (Widget xs) m a
freeze w a = FreezeT $ return $ Freeze w a

freeze' :: Monad m => Widget xs -> FreezeT (Widget xs) m ()
freeze' w = freeze w ()

freezeM :: Monad m => m (Widget xs) -> a -> FreezeT (Widget xs) m a
freezeM mw a = FreezeT $ fmap (\w -> Freeze w a) mw

freezeM' :: Monad m => m (Widget xs) -> FreezeT (Widget xs) m ()
freezeM' w = freezeM w ()

override :: (Widget old -> Widget new) -> Widget old -> (forall br m. Union new br m ~> (br (Widget new) m `Sum` Union old br m)) -> Widget new
override updater wx fu = Widget $ elim id (bimapT updater id . runWidget wx) . fu where
  elim :: (f ~> r) -> (g ~> r) -> (f `Sum` g ~> r)
  elim f g x = case x of
    InL a -> f a
    InR a -> g a

type family (++) (a :: [k]) (b :: [k]) where
  '[] ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)

onFreeze :: (TransBifunctor FreezeT m, Monad m)
  => Lens' s (Widget xs) -> (Widget xs -> FreezeT (Widget xs) Identity a) -> s -> (a -> Widget xs -> m s) -> m s
onFreeze lens op s cb = do
  case runIdentity $ runFreezeT $ op (s^.lens) of
    Freeze w a -> cb a w
    Keep w -> return $ s & lens .~ w

onFreeze' :: (TransBifunctor FreezeT m, Monad m)
  => Lens' s (Widget xs) -> (Widget xs -> FreezeT (Widget xs) Identity ()) -> s -> (Widget xs -> m s) -> m s
onFreeze' lens op s cb = onFreeze lens op s (\_ -> cb)

onFinish :: Monad m
         => Lens' s (Widget xs) -> (Widget xs -> FreezeT (Widget xs) Identity a) -> s -> (a -> s -> m s) -> m s
onFinish lens op s cb = onFreeze lens op s (\a w -> cb a (s & lens .~ w))

onFinishM :: (TransBifunctor FreezeT m, Monad m)
          => Lens' s (Widget xs) -> (Widget xs -> m (FreezeT (Widget xs) Identity a)) -> s -> (a -> s -> m s) -> m s
onFinishM lens op s cb = do
  fw <- fmap (runIdentity . runFreezeT) $ op (s^.lens)
  case fw of
    Freeze w a -> cb a (s & lens .~ w)
    Keep w -> return $ s & lens .~ w

runSwitch :: Widget xs -> Getter (Widget xs) (FreezeT (Widget xs) Identity a) -> (Freeze (Widget xs) a -> r) -> r
runSwitch w op k = k $ runIdentity $ runFreezeT (w ^. op)

runSwitchM :: (k ∈ xs, Monad m) => Widget xs -> k FreezeT m a -> (Freeze (Widget xs) a -> m r) -> m r
runSwitchM w op k = runFreezeT (w `call` op) >>= k

op'isFreeze :: Widget xs -> Getter (Widget xs) (FreezeT (Widget xs) Identity a) -> Bool 
op'isFreeze w op = runSwitch w op isFreeze

--

wEmpty :: Widget '[]
wEmpty = Widget emptyUnion

