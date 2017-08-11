{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Extensible.Config where

import Control.Lens
import Data.Extensible
import GHC.TypeLits

class Default (symbol :: Symbol) where
  type Optional symbol :: [Assoc Symbol *]
  def :: proxy symbol -> Record (Optional symbol)

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

--cfgs :: (Default d, IncludeAssoc ys xs) => Iso' d (Record ys) -> Record xs -> d
--cfgs wr hx = def & wr %~ hmergeAssoc hx

