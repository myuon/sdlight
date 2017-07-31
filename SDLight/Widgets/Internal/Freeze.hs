{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SDLight.Widgets.Internal.Freeze where

import Control.Lens
import SDLight.Widgets.Internal.Widget

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

newtype FreezeT w m a = FreezeT { runFreezeT :: m (Freeze w a) }

instance Functor m => TransBifunctor FreezeT m where
  bimapT f g = FreezeT . fmap (unfreeze (\a c -> Freeze (f a) (g c)) (Keep . f)) . runFreezeT

instance NodeW FreezeT where
  continue = FreezeT . return . Keep
  continueM = FreezeT . fmap Keep

  _self opr = to $ \w -> fmap (^._Frozen) $ runFreezeT $ w `call` opr

freeze :: Monad m => Widget xs -> a -> FreezeT (Widget xs) m a
freeze w a = FreezeT $ return $ Freeze w a

freeze' :: Monad m => Widget xs -> FreezeT (Widget xs) m ()
freeze' w = freeze w ()

freezeM :: Monad m => m (Widget xs) -> a -> FreezeT (Widget xs) m a
freezeM mw a = FreezeT $ fmap (\w -> Freeze w a) mw

freezeM' :: Monad m => m (Widget xs) -> FreezeT (Widget xs) m ()
freezeM' w = freezeM w ()


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


