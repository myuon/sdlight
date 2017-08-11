{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Extensible.Config where

import Control.Lens
import Data.Extensible
import GHC.TypeLits

type family FilterAssoc (xs :: [Assoc k *]) (ys :: [k]) :: [Assoc k *] where
  FilterAssoc ((k >: v) : kvs) (k : ks) = k >: v : FilterAssoc kvs ks
  FilterAssoc ((_ >: v) : kvs) (k : ks) = FilterAssoc kvs (k : ks)
  FilterAssoc kvs '[] = '[]

class Default (symbol :: Symbol) xs | symbol -> xs where
  type Optional symbol :: [Symbol]
  def :: Record (FilterAssoc xs (Optional symbol))

data WidgetId
  = WId String
  | Wapp WidgetId WidgetId
  | WEmpty
  deriving Eq

instance Show WidgetId where
  show (WId n) = n
  show (Wapp x y) = show x ++ " </> " ++ show y
  show WEmpty = "<>"

instance Monoid WidgetId where
  mempty = WEmpty
  mappend = Wapp

infixl 1 </>
(</>) :: WidgetId -> WidgetId -> WidgetId
(</>) = mappend

idSymbols :: WidgetId -> [String]
idSymbols (WId a) = [a]
idSymbols (Wapp x y) = idSymbols x ++ idSymbols y
idSymbols WEmpty = []

type Wix xs = "wix" >: WidgetId : xs
type WConfig xs = Record (Wix xs)

type HasWix xs = Associate "wix" WidgetId (Wix xs)

giveWid :: HasWix cfg => String -> WConfig cfg -> WConfig cfg
giveWid w wcfg = wcfg & #wix %~ (</> WId w)

-- extensible

hmergePlain :: (xs ⊆ ys, Wrapper h) => h :* xs -> h :* ys -> h :* ys
hmergePlain hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusion) .~ x^._Wrapper) hy hx

hmerge :: (IncludeAssoc ys xs, Wrapper h) => h :* xs -> h :* ys -> h :* ys
hmerge hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusionAssoc) .~ x^._Wrapper) hy hx

unsafeExpand :: (IncludeAssoc ys xs, Wrapper h, Generate ys) => h :* xs -> h :* ys
unsafeExpand hx = let bottom = hrepeat undefined in hmerge hx bottom

