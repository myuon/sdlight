{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
module SDLight.Stylesheet where

import Control.Lens
import Control.Applicative
import qualified Data.Map as M
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

data StyleAttr = Padding (V2 Int) | Margin (V2 Int) | Width Int | Height Int
  deriving (Eq, Ord, Show)

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

newtype StyleSyntax = StyleSyntax { getStyleSyntax :: Tree StyleQuery [StyleAttr] }
  deriving (Eq, Show)

pstylesheet :: Parser StyleSyntax
pstylesheet = StyleSyntax . Node Wild <$> expr where
  expr :: Parser [Tree StyleQuery [StyleAttr]]
  expr = many $ do
    sid <- spaces *> pselector <* spaces
    ats <- between (symbol "{") (symbol "}") $ try expr <|> (return . Leaf <$> fmap (map toStyleAttr) pattrs)
    return $ Node sid ats
  
  pselector :: Parser StyleQuery
  pselector = try wild <|> deps where
    wild = symbol "_" *> return Wild
    deps = do
      a <- (some (letter <|> oneOf "-")) <* spaces
      try (fmap (a :>:) $ symbol ">" *> deps)
        <|> try (fmap (a :>>:) $ symbol ">>" *> deps)
        <|> return (StyleId a)

  pattrs :: Parser [(String, [Int])]
  pattrs = many $ do
    attr <- some letter
    spaces *> symbol ":"
    value <- natural `sepBy` symbol ","

    return $ (attr, fmap fromInteger value)

  toStyleAttr :: (String, [Int]) -> StyleAttr
  toStyleAttr ("padding", [x,y]) = Padding (V2 x y)
  toStyleAttr ("margin", [x,y]) = Margin (V2 x y)
  toStyleAttr ("width", [x]) = Width x
  toStyleAttr ("height", [x]) = Height x

loadStyleFile :: FilePath -> IO (Maybe StyleSheet)
loadStyleFile = fmap (fmap fromSyntax) <$> parseFromFile pstylesheet

newtype StyleSheet = StyleSheet { getStyleSheet :: M.Map StyleQuery [StyleAttr] }
  deriving (Eq, Show)

fromSyntax :: StyleSyntax -> StyleSheet
fromSyntax (StyleSyntax syntax) = StyleSheet $ go Wild syntax where
  go :: StyleQuery -> Tree StyleQuery a -> M.Map StyleQuery a
  go k (Leaf a) = M.singleton k a
  go k (Node k' xs) = foldr (M.union . go (k `mappend` k')) M.empty xs

wix :: WidgetId -> Getter StyleSheet [(StyleQuery, [StyleAttr])]
wix w = to $ \sty -> M.assocs $ M.filterWithKey (\q _ -> match q (toIdListL w)) $ getStyleSheet sty where
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

