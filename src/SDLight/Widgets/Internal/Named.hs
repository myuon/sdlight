{-# LANGUAGE FlexibleInstances #-}
{-|
Named Widget
-}
module SDLight.Widgets.Internal.Named where

import Control.Lens
import Data.Extensible
import SDLight.Stylesheet
import SDLight.Widgets.Internal.Widget
import SDLight.Widgets.Internal.TH

makeOp "GetName" [t| _ Value Identity WidgetId |]

-- | @Named xs@ is a method list which contains @Op'GetName@
type Named xs = Op'GetName : xs

-- | @NamedWidget xs@ widget has its own name with a method list @xs@
type NamedWidget xs = Widget (Named xs)

-- | named widget
--
-- == Methods
--
-- * 'op'getName'
wNamed :: WidgetId -> Widget xs -> Widget (Named xs)
wNamed w xs = Widget $ (\(Op'GetName) -> finish w) @> (bimapT (wNamed w) id . runWidget xs)

-- | Get its widget id from a widget
-- Returns @Nothing@ if @xs@ does not have 'Op'GetName' method
class KnownName xs where
  symbolName :: Widget xs -> Maybe WidgetId

instance KnownName xs where
  symbolName _ = Nothing

instance {-# OVERLAPPING #-} (FindType Op'GetName xs ~ '[]) => KnownName (Op'GetName : xs) where
  symbolName w = Just $ w^.op'getName

