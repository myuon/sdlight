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
module SDLight.Widgets.Core where
{-
  ( type (~>)
  , type (~~>)
  , type (∈)
  , type (⊆)
  , Union(..)
  , Member
  , type (++)
  , type (:*)
  , type (:$)
  , (@>)
  , emptyUnion
  
  , Eff(..)
  , (@>>)
  , emptyEff
  , (@!)
  , (@!!)
  , (@++@)

  , (@@~)

  , Op'Render(..)
  , Op'Run(..)
  , Op'Reset(..)
  , Op'HandleEvent(..)
  , Op'Lift(..)

  , type Extends
  , type Extended
  , type (:<?)
  , type (:<@)
  , extend
  , (@:<@)
  ) where
-}
  
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

data Union (r :: [* -> *]) v where
  UNow  :: t v -> Union (t : r) v
  UNext :: Union r v -> Union (any : r) v

class Member ts x where
  liftU :: (x ~> r) -> (Union ts ~> r) -> (Union ts ~> r)
  inj :: x ~> Union ts

instance {-# OVERLAPPING #-} Member (t : ts) t where
  liftU f g (UNow tv) = f tv
  inj = UNow
  
instance {-# OVERLAPPABLE #-} Member ts t => Member (any : ts) t where
  liftU f g (UNow av) = g (UNow av)
  liftU f g (UNext t) = liftU f (g . UNext) t

  inj xv = UNext (inj xv)

type (∈) x xs = Member xs x

class CaseOf r t rs | r t -> rs where
  caseOf :: Union r v -> Either (Union rs v) (t v)

instance CaseOf (r : rs) r rs where
  caseOf (UNow tv) = Right tv
  caseOf (UNext n) = Left n

infixr 2 @>
(@>) :: (t ~> r) -> (Union ts ~> r) -> Union (t : ts) ~> r
(@>) f g u = either g f $ caseOf u

emptyUnion :: Union '[] v -> a
emptyUnion = \case

--

newtype Widget ops m = Widget { runWidget :: Union ops ~> EitherT (Widget ops m) m }

data Op'Render r where
  Op'Render :: SDL.V2 Int -> Op'Render ()

data Op'Run r where
  Op'Run :: Op'Run Void

data Op'Reset r where
  Op'Reset :: Op'Reset Void

data Op'HandleEvent r where
  Op'HandleEvent :: M.Map SDL.Scancode Int -> Op'HandleEvent Void

call :: (k ∈ xs, Monad m) => Widget xs m -> (forall v. k v -> m (Either (Widget xs m) v))
call w op = runEitherT $ runWidget w (inj op)

infixl 4 @!
(@!) :: (k ∈ xs, Monad m) => Widget xs m -> (forall v. k v -> m (Either (Widget xs m) v))
w @! op = runEitherT $ runWidget w (inj op)

infixl 4 @!!
(@!!) :: (k ∈ xs, Monad m) => m (Widget xs m) -> (forall v. k v -> m (Either (Widget xs m) v))
mw @!! op = mw >>= \w -> w @! op

infixl 4 @.
(@.) :: (k ∈ xs, Monad m) => Widget xs m -> k Void -> m (Widget xs m)
w @. op = w @! op >>= \case
  Left w' -> return w'
  Right v -> absurd v

infixl 4 @..
(@..) :: (k ∈ xs, Monad m) => m (Widget xs m) -> k Void -> m (Widget xs m)
mw @.. op = mw >>= \w -> w @. op

data Incr a where Incr :: Incr Void
data Print a where Print :: Print ()
data IsMod5 a where IsMod5 :: IsMod5 Bool

wcounter :: Int -> Widget [Incr, Print, IsMod5] IO
wcounter n = Widget $
  (\Incr -> left $ wcounter (n+1))
  @> (\Print -> lift $ print n)
  @> (\IsMod5 -> right $ n `mod` 5 == 0)
  @> emptyUnion

greetEvery5 :: Widget [Incr, Print, IsMod5] IO -> Widget '[Op'Run] IO
greetEvery5 w = Widget $
  (\Op'Run -> EitherT $ do
      w' <- w @. Incr
      Right isMod5 <- w' @! IsMod5
      when isMod5 $ do
        print "hey!!!"
      
      return $ Left $ greetEvery5 w'
      )
  @> emptyUnion

main = do
  let w = greetEvery5 $ wcounter 0
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run

  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  Left w <- w @! Op'Run
  
  return ()


{-
type (~~>) f g = forall x y. f x y -> g x y

infix 1 @@~
(@@~) :: Monad m => m s -> (t ~> StateT s m) -> Object t m
ms @@~ k = Object $ \t -> ms >>= \s -> (s @~ k) @- t

--

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
(@>) :: (t ~> r) -> (Union ts ~> r) -> Union (t : ts) ~> r
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
type family (:$) (xs :: [* -> (* -> *) -> * -> *]) (a :: *) = r | r -> xs where
  '[] :$ a = '[]
  (x : xs) :$ a = x a : (xs :$ a)

--

type Eff ts m = Object (Union ts) m

singletonE :: Functor m => (t ~> m) -> Eff '[t] m
singletonE f = liftO $ \case
  UNow xv -> f xv
  UNext t -> emptyUnion t
  
tailE :: Functor m => Eff (x : xs) m -> Eff xs m
tailE ef = Object $ \u -> second tailE <$> runObject ef (UNext u)

emptyEff :: Eff '[] m
emptyEff = Object emptyUnion

infixr 2 @>>
(@>>) :: Functor m => (x ~> m) -> Eff xs m -> Eff (x : xs) m
f @>> ef = Object $ \case
  (UNow xv) -> (\x -> (x, f @>> ef)) <$> f xv
  (UNext t) -> second (f @>>) <$> runObject ef t

undefinedConsE :: Functor m => Eff xs m -> Eff (x : xs) m
undefinedConsE ef = Object $ \(UNext u) -> second undefinedConsE <$> runObject ef u

class SumU xs ys where
  (@++@) :: Functor m => Eff xs m -> Eff ys m -> Eff (xs ++ ys) m

instance SumU '[] ys where
  xs @++@ ys = ys

instance SumU xs ys => SumU (x : xs) ys where
  xs @++@ ys = Object $ \case
    UNow xv -> second (@++@ ys) <$> runObject xs (UNow xv)
    UNext t -> second undefinedConsE <$> runObject (tailE xs @++@ ys) t

infix 6 @!
(@!) :: Member xs x => Eff xs m -> (forall v. x v -> m (v, Eff xs m))
ef @! method = runObject ef (inj method)

infix 6 @!!
(@!!) :: (Member xs x, Functor m) => Eff xs m -> (x ~> m)
ef @!! method = fst <$> runObject ef (inj method)

--

-- lift

data Op'Lift k r where
  Op'Lift :: k r -> Op'Lift k r

class EffLift xs where
  oplift :: Functor m => Eff xs m -> Eff (Op'Lift :* xs) m

instance EffLift '[] where
  oplift = id

instance EffLift xs => EffLift (x : xs) where
  oplift xs = Object $ \case
    UNow (Op'Lift xv) -> second oplift <$> xs @! xv
    UNext t -> second undefinedConsE <$> (oplift (tailE xs) @- t)

--

type Extends xs ys = (SumU xs (Op'Lift :* ys), EffLift ys)
type Extended xs ys = xs ++ (Op'Lift :* ys)

type (:<?) as b = Extends '[b] as
type (:<@) xs y = Extended '[y] xs

extend :: (Functor m, xs `Extends` ys) => Eff ys m -> Eff xs m -> Eff (xs `Extended` ys) m
extend ys xs = xs @++@ oplift ys

(@:<@) :: (Functor m, xs :<? y) => Eff xs m -> Eff '[y] m -> Eff (xs :<@ y) m
(@:<@) = extend

newtype Ref r v = Ref (MVar r,v)

instance Wrapped (Ref r v) where
  type Unwrapped (Ref r v) = (MVar r,v)
  _Wrapped' = iso (\(Ref m) -> m) Ref

_ref :: Lens' (Ref r v) (MVar r)
_ref = _Wrapped'._1

_refto :: Lens' (Ref r v) v
_refto = _Wrapped'._2

obj1 :: Eff '[Op'Run, Op'Render] IO
obj1 = ((10 :: Int) @~) $
  (\Op'Run -> lift (print "obj1!!") >> id %= (+2))
  @> (\(Op'Render _) -> get >>= \n -> lift $ print (n :: Int))
  @> emptyUnion

objExt :: (xs :<? y) => Eff xs IO -> Eff (xs :<@ Op'Run) IO
objExt ef = (ef @:<@) $ "const: 42" @~
  (\Op'Run -> do
     lift $ print "objExt"
     id %= (++ ".")
  )
  @> emptyUnion

main = do
  print 10

  k <- new obj1
  k .- inj Op'Run
  k .- inj (Op'Render 10)

  m <- new (objExt obj1)
  m .- inj Op'Run
  m .- inj (Op'Lift $ Op'Render 0)
  m .- inj Op'Run
  m .- inj (Op'Lift $ Op'Render 0)
  m .- inj Op'Run
  m .- inj (Op'Lift $ Op'Render 0)
  m .- inj Op'Run
  m .- inj (Op'Lift $ Op'Render 0)
-}


