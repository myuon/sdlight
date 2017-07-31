{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
module SDLight.Stylesheet where

import Control.Applicative
import qualified Data.Map as M
import Data.Reflection
import Data.Proxy
import Text.Trifecta

data WidgetId
  = WClass String
  | WId String
  | Wapp WidgetId WidgetId
  | WEmpty
  deriving Eq

instance Show WidgetId where
  show (WClass n) = "." ++ n
  show (WId n) = "#" ++ n
  show (Wapp x y) = show x ++ " </> " ++ show y
  show WEmpty = "<>"

newtype WPath = WPath { unwrapWPath :: forall r. (forall s. Reifies s WidgetId => Proxy s -> r) -> r }

instance Monoid WPath where
  mempty = WPath $ reify WEmpty
  mappend (WPath px) (WPath py) = WPath $ \k -> reify (Wapp (px reflect) (py reflect)) k

infixl 1 </>
(</>) :: WPath -> WPath -> WPath
(</>) = mappend

getWPath :: WPath -> WidgetId
getWPath (WPath p) = p reflect

giveWId :: String -> WPath
giveWId s = WPath $ reify (WId s)

giveWClass :: String -> WPath
giveWClass s = WPath $ reify (WClass s)

data StyleAttr = Padding | Margin | Width | Height
  deriving (Eq, Ord, Show)

instance Read StyleAttr where
  readsPrec _ "padding" = [(Padding, "")]
  readsPrec _ "margin" = [(Margin, "")]
  readsPrec _ "width" = [(Width, "")]
  readsPrec _ "height" = [(Height, "")]

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

newtype StyleSyntax = StyleSyntax { getStyleSyntax :: Tree StyleQuery [(StyleAttr,Int)] }
  deriving (Eq, Show)

pstylesheet :: Parser StyleSyntax
pstylesheet = StyleSyntax . Node Wild <$> expr where
  expr :: Parser [Tree StyleQuery [(StyleAttr,Int)]]
  expr = many $ do
    sid <- spaces *> pselector <* spaces
    ats <- between (symbol "{") (symbol "}") $ try expr <|> (return . Leaf <$> pattrs)
    return $ Node sid ats
  
  pselector :: Parser StyleQuery
  pselector = try wild <|> deps where
    wild = symbol "_" *> return Wild
    deps = do
      a <- some letter <* spaces
      try (fmap (a :>:) $ symbol ">" *> deps)
        <|> try (fmap (a :>>:) $ symbol ">>" *> deps)
        <|> return (StyleId a)

  pattrs :: Parser [(StyleAttr,Int)]
  pattrs = many $ do
    attr <- some letter
    spaces *> symbol ":"
    value <- natural

    return $ (read attr, fromInteger value)

loadStyleFile :: FilePath -> IO (Maybe StyleSheet)
loadStyleFile = fmap (fmap fromSyntax) <$> parseFromFile pstylesheet

newtype StyleSheet = StyleSheet { getStyleSheet :: M.Map StyleQuery [(StyleAttr, Int)] }
  deriving (Eq, Show)

fromSyntax :: StyleSyntax -> StyleSheet
fromSyntax (StyleSyntax syntax) = StyleSheet $ go Wild syntax where
  go :: StyleQuery -> Tree StyleQuery a -> M.Map StyleQuery a
  go k (Leaf a) = M.singleton k a
  go k (Node k' xs) = foldr (M.union . go (k `mappend` k')) M.empty xs

