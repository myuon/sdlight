{-# LANGUAGE FlexibleInstances #-}
module SDLight.Widgets.Internal.Named where

import Control.Lens
import Data.Extensible
import SDLight.Stylesheet
import SDLight.Widgets.Internal.Widget
import SDLight.Widgets.Internal.TH

makeOp "GetName" [t| _ Value Identity WidgetId |]

type Named xs = Op'GetName : xs
type NamedWidget xs = Widget (Named xs)

wNamed :: WidgetId -> Widget xs -> Widget (Named xs)
wNamed w xs = Widget $ (\(Op'GetName) -> finish w) @> (bimapT (wNamed w) id . runWidget xs)

class KnownName xs where
  symbolName :: Widget xs -> Maybe WidgetId

instance KnownName xs where
  symbolName _ = Nothing

instance {-# OVERLAPPING #-} (FindType Op'GetName xs ~ '[]) => KnownName (Op'GetName : xs) where
  symbolName w = Just $ w^.op'getName

