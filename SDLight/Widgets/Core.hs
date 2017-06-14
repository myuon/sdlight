{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module SDLight.Widgets.Core where

import Pipes
import Control.Object
import Control.Monad.State.Strict
import Control.Monad.Skeleton
import Data.Functor.Identity
import Data.Functor.Sum

type Widget eff m r = Monad m => forall r. Pipe (eff m r) r m ()

--

data Child1 = Child1 Int deriving (Eq, Show)
data Child2 = Child2 String deriving (Eq, Show)

data Add = Add Int

data Eff'Child1 m r where
  Render'Child1 :: Eff'Child1 IO ()
  Step'Child1 :: Eff'Child1 IO Add

objChild1 :: Monad m => Object (Eff'Child1 m) m
objChild1 = stateful go (Child1 299) where
  go :: Eff'Child1 m r -> StateT Child1 m r
  go Render'Child1 = get >>= lift . print
  go Step'Child1 = do
    Child1 n <- get
    put $ Child1 (n+1)
    return $ Add n

data Eff'Child2 m r where
  Render'Child2 :: Eff'Child2 IO ()
  Step'Child2 :: Eff'Child2 IO ()

objChild2 :: Monad m => Object (Eff'Child2 m) m
objChild2 = stateful go (Child2 "poyo") where
  go :: Eff'Child2 m r -> StateT Child2 m r
  go Render'Child2 = get >>= lift . print
  go Step'Child2 = do
    Child2 n <- get
    put $ Child2 (n ++ ".")

--

data Eff'Parent m r where
  Render'Parent :: Eff'Parent IO ()
  Step'Parent :: Eff'Parent IO ()

data Parent m
  = Parent
  { count :: Int
  , child1 :: Instance (Eff'Child1 m) m
  , child2 :: Instance (Eff'Child2 m) m
  }

objParent :: Monad m => Parent m -> Object (Eff'Parent m) m
objParent = stateful go where
  go :: Eff'Parent m r -> StateT (Parent m) m r
  go Render'Parent = do
    this <- get
    lift $ print $ "parent render:"
    lift $ print $ count this
    lift $ print $ "children render:"
    lift $ child1 this .- Render'Child1
    lift $ child2 this .- Render'Child2
  go Step'Parent = do
    this <- get
    Add n <- lift $ child1 this .- Step'Child1
    modify $ \t -> t { count = count t + n }

    lift $ child2 this .- Step'Child2

    modify $ \t -> t { count = count t + 1 }

main = do
  ch1 <- new objChild1
  ch2 <- new objChild2
  parent <- new $ objParent $ Parent 5 ch1 ch2

  parent .- Render'Parent
  parent .- Step'Parent
  parent .- Render'Parent
  parent .- Step'Parent
  parent .- Render'Parent

