{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
module SDLight.Stylesheet where

import Control.Lens
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe
import Text.Trifecta
import Linear.V2

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

toIdListL :: WidgetId -> [String]
toIdListL (WId a) = [a]
toIdListL (Wapp x y) = toIdListL x ++ toIdListL y
toIdListL WEmpty = []

data StyleAttr r where
  Padding :: StyleAttr (V2 Int)
  Margin :: StyleAttr (V2 Int)
  Width :: StyleAttr Int
  Height :: StyleAttr Int

data StyleAttrValue = forall r. StyleAttrValue { getStyleAttrValue :: (StyleAttr r, r) }

data Tree k a
  = Leaf a
  | Node k [Tree k a]
  deriving (Eq, Show, Foldable)

data StyleQuery
  = Wild
  | StyleId String
  | String :>: StyleQuery
  | String :>>: StyleQuery
  deriving (Eq, Ord, Show)

instance Monoid StyleQuery where
  mempty = Wild
  mappend Wild y = y
  mappend x Wild = x
  mappend (StyleId s) y = s :>>: y
  mappend (s :>: sx) y = s :>: mappend sx y
  mappend (s :>>: sx) y = s :>>: mappend sx y

newtype StyleSyntax = StyleSyntax { getStyleSyntax :: Tree StyleQuery [StyleAttrValue] }

pstylesheet :: Parser StyleSyntax
pstylesheet = StyleSyntax . Node Wild <$> expr where
  expr :: Parser [Tree StyleQuery [StyleAttrValue]]
  expr = many $ do
    sid <- spaces *> pselector <* spaces
    ats <- between (symbol "{") (symbol "}") $ try expr <|> (return . Leaf <$> pattrs)
    return $ Node sid ats
  
  pselector :: Parser StyleQuery
  pselector = try wild <|> deps where
    wild = symbol "_" *> return Wild
    deps = do
      a <- (some (letter <|> oneOf "-")) <* spaces
      try (fmap (a :>:) $ symbol ">" *> deps)
        <|> try (fmap (a :>>:) $ symbol ">>" *> deps)
        <|> return (StyleId a)

  pattrs :: Parser [StyleAttrValue]
  pattrs = many $ do
    attr <- some letter
    spaces *> symbol ":"
    value <- natural `sepBy` symbol ","

    return $ toStyleAttr (attr, fmap fromInteger value)

  toStyleAttr :: (String, [Int]) -> StyleAttrValue
  toStyleAttr ("padding", [x,y]) = StyleAttrValue $ (,) Padding (V2 x y)
  toStyleAttr ("margin", [x,y]) = StyleAttrValue $ (,) Margin (V2 x y)
  toStyleAttr ("width", [x]) = StyleAttrValue $ (,) Width x
  toStyleAttr ("height", [x]) = StyleAttrValue $ (,) Height x

loadStyleFile :: FilePath -> IO (Maybe StyleSheet)
loadStyleFile = fmap (fmap fromSyntax) <$> parseFromFile pstylesheet

newtype StyleSheet = StyleSheet { getStyleSheet :: M.Map StyleQuery [StyleAttrValue] }

fromSyntax :: StyleSyntax -> StyleSheet
fromSyntax (StyleSyntax syntax) = StyleSheet $ go Wild syntax where
  go :: StyleQuery -> Tree StyleQuery a -> M.Map StyleQuery a
  go k (Leaf a) = M.singleton k a
  go k (Node k' xs) = foldr (M.union . go (k `mappend` k')) M.empty xs

wix :: WidgetId -> StyleAttr r -> Getter StyleSheet (Maybe r)
wix w attr = to $ \sty -> (\xs -> if null xs then Nothing else Just $ last xs) $ catMaybes $ fmap (matchAttr attr) $ concat $ M.elems $ M.filterWithKey (\q _ -> match q (toIdListL w)) $ getStyleSheet sty where
  match :: StyleQuery -> [String] -> Bool
  match q w | matchHere q w = True
  match (StyleId s) (x:ys) = match (StyleId s) ys
  match (q :>: ps) (x:ys) = match (q :>: ps) ys
  match (q :>>: ps) (x:ys) = match (q :>: ps) ys
  match _ _ = False

  matchHere Wild w = True
  matchHere (StyleId q) (x:_) = q == x
  matchHere (q :>: ps) (x:ys) | q == x = matchHere ps ys
  matchHere (q :>>: ps) (x:ys) | q == x = match ps ys
  matchHere _ _ = False

  matchAttr :: StyleAttr r -> StyleAttrValue -> Maybe r
  matchAttr attr v = case (attr, v) of
    (Padding, StyleAttrValue (Padding,v)) -> Just v
    (Margin, StyleAttrValue (Margin,v)) -> Just v
    (Width, StyleAttrValue (Width,v)) -> Just v
    (Height, StyleAttrValue (Height,v)) -> Just v
    _ -> Nothing

