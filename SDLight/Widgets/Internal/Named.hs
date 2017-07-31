{-# LANGUAGE FlexibleInstances #-}
module SDLight.Widgets.Internal.Named where

import Control.Lens
import SDLight.Stylesheet
import SDLight.Widgets.Internal.Widget
import SDLight.Widgets.Internal.TH

makeOp "GetName" [t| _ Value Identity WidgetId |]

type Named xs = Op'GetName : xs
type NamedWidget xs = Widget (Named xs)

wNamed :: WidgetId -> Widget xs -> Widget (Named xs)
wNamed w xs = Widget $ either (bimapT (wNamed w) id . runWidget xs) (\Op'GetName -> finish w) . caseOf

class KnownName xs where
  symbolName :: Widget xs -> Maybe WidgetId

instance KnownName xs where
  symbolName _ = Nothing

instance {-# OVERLAPPING #-} KnownName (Op'GetName : xs) where
  symbolName w = Just $ w^.op'getName

