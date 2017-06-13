{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module SDLight.Widgets.Core where

import Pipes

type Widget eff m r = Monad m => forall r. Pipe (eff m r) r m ()

