{-# LANGUAGE DeriveFoldable #-}
module SDLight.Stylesheet where

import Control.Applicative
import qualified Data.Map as M
import Text.Trifecta

data StyleId a
  = EmptyId
  | a :#>: (StyleId a)
  | Wild
  deriving (Eq, Ord, Show)

data StyleAttr = Padding | Margin | Width | Height
  deriving (Eq, Ord, Show)

data Tree k a
  = Leaf a
  | Node k [Tree k a]
  deriving (Eq, Show, Foldable)

newtype StyleSyntax = StyleSyntax { getStyleSyntax :: Tree [String] [(StyleAttr,Int)] }
  deriving (Eq, Show)

pstylesheet :: Parser StyleSyntax
pstylesheet = StyleSyntax . Node [] <$> expr where
  expr :: Parser [Tree [String] [(StyleAttr,Int)]]
  expr = many $ do
    sid <- spaces *> pselector <* spaces
    ats <- between (symbol "{") (symbol "}") $ try expr <|> (return . Leaf <$> pattrs)
    return $ Node sid ats
  
  pselector :: Parser [String]
  pselector = try wild <|> deps where
    wild = symbol "_" *> return []
    deps = do
      a <- some letter <* spaces
      as <- try (symbol ">" *> deps) <|> return []
      return $ a : as

  toAttr :: String -> StyleAttr
  toAttr "padding" = Padding
  toAttr "margin" = Margin
  toAttr "width" = Width
  toAttr "height" = Height

  pattrs :: Parser [(StyleAttr,Int)]
  pattrs = many $ do
    attr <- some letter
    spaces *> symbol ":"
    value <- natural

    return $ (toAttr attr, fromInteger value)

loadStyleFile :: FilePath -> IO (Maybe StyleSheet)
loadStyleFile = fmap (fmap fromSyntax) <$> parseFromFile pstylesheet

newtype StyleSheet = StyleSheet { getStyleSheet :: M.Map [String] [(StyleAttr, Int)] }
  deriving (Eq, Ord, Show)

fromSyntax :: StyleSyntax -> StyleSheet
fromSyntax (StyleSyntax syntax) = StyleSheet $ go [] syntax where
  go :: [String] -> Tree [String] a -> M.Map [String] a
  go k (Leaf a) = M.singleton k a
  go k (Node k' xs) = foldr (\t m -> M.union (go (k ++ k') t) m) M.empty xs

