{-|
Builder by TH
-}
module SDLight.Widgets.Internal.TH
  ( makeOp
  ) where

import Control.Lens
import Control.Monad
import Data.Char
import Language.Haskell.TH
import SDLight.Widgets.Internal.Widget

data Operator
  = Operator
  { _args :: [Type]
  , _name :: Name
  , _branch :: Type
  , _monad :: Type
  , _rettype :: Type
  }

makeLenses ''Operator

buildArr :: [Type] -> Type -> Type
buildArr xs base = foldr (\a b -> AppT (AppT ArrowT a) b) base xs

parseOpr :: Type -> Operator
parseOpr = getArgs getOpTypes
  where
    getArgs :: (Type -> Operator) -> Type -> Operator
    getArgs k (AppT (AppT ArrowT a) b) = k b & args %~ (a :)
    getArgs k u = k u

    getOpTypes :: Type -> Operator
    getOpTypes (AppT (ConT n) (AppT br (AppT m r))) = Operator [] n br m r

declareOpDatatype :: Operator -> DecsQ
declareOpDatatype opr = do
  brName <- newName "br"
  mName <- newName "m"
  rName <- newName "r"

  {- data Op'K br m r where
       Op'K :: args -> Op'K br m r -}
  return $
    [ DataD [] (opr^.name) [PlainTV brName, PlainTV mName, PlainTV rName] Nothing
      [ GadtC [opr^.name] []
      $ buildArr (opr^.args)
      $ ConT (opr^.name) `AppT` (opr^.branch) `AppT` (opr^.monad) `AppT` (opr^.rettype)
      ] []
    ]

declareOpGetter :: Operator -> DecsQ
declareOpGetter opr = case opr^.branch of
  ConT c | c == ''Self -> declareOpSelfGetter opr
  ConT c | c == ''Value -> declareOpValueGetter opr
  _ -> declareOpOtherGetter opr

  where
    declareOpOtherGetter :: Operator -> DecsQ
    declareOpOtherGetter opr = do
      xsName <- newName "xs"
      let opName = mkName $ (\s -> let (a,b) = splitAt 4 s in fmap toLower a ++ b) $ show $ opr^.name

      {- op'piyo :: Op'Piyo ∈ xs => A -> B -> Getter (Widget xs) (FreezeT (Widget xs) Identity ())
         op'piyo a b = _Op (Op'Piyo a b) -}
      let wxs = ConT (mkName "Widget") `AppT` VarT xsName
      vars <- replicateM (length $ opr^.args) $ newName "x"

      return $
        [ SigD opName
          $ ForallT [] [(ConT (mkName "∈")) `AppT` ConT (opr^.name) `AppT` VarT xsName]
          $ buildArr (opr^.args)
          $ ConT (mkName "Getter")
          `AppT` wxs
          `AppT` ((opr^.branch) `AppT` wxs `AppT` (opr^.monad) `AppT` (opr^.rettype))
        , FunD opName
          [ Clause
            (fmap VarP vars)
            (NormalB $ VarE (mkName "_Op") `AppE` foldl (\e n -> e `AppE` VarE n) (ConE (opr^.name)) vars)
            []
          ]
        ]

    declareOpSelfGetter :: Operator -> DecsQ
    declareOpSelfGetter opr = do
      let isIdentity = opr^.monad == ConT ''Identity
      xsName <- newName "xs"
      let opName = mkName $ (\s -> let (a,b) = splitAt 4 s in fmap toLower a ++ b) $ show $ opr^.name

      {- op'piyo :: Op'Piyo ∈ xs => A -> B -> Getter (Widget xs) (m (Widget xs))
         op'piyo a b = _self (Op'Piyo a b) -}
      let wxs = ConT (mkName "Widget") `AppT` VarT xsName
      vars <- replicateM (length $ opr^.args) $ newName "x"

      return $
        [ SigD opName
          $ ForallT [] [(ConT (mkName "∈")) `AppT` ConT (opr^.name) `AppT` VarT xsName]
          $ buildArr (opr^.args)
          $ ConT (mkName "Getter")
          `AppT` wxs
          `AppT` (if isIdentity then id else AppT (opr^.monad)) wxs
        , FunD opName
          [ Clause
            (fmap VarP vars)
            (NormalB $ VarE (mkName $ if isIdentity then "_self'" else "_self") `AppE` foldl (\e n -> e `AppE` VarE n) (ConE (opr^.name)) vars)
            []
          ]
        ]

    declareOpValueGetter :: Operator -> DecsQ
    declareOpValueGetter opr = do
      let isIdentity = opr^.monad == ConT ''Identity
      xsName <- newName "xs"
      let opName = mkName $ (\s -> let (a,b) = splitAt 4 s in fmap toLower a ++ b) $ show $ opr^.name

      {- op'piyo :: Op'Piyo ∈ xs => A -> B -> Getter (Widget xs) (m r)
         op'piyo a b = _value (Op'Piyo a b) -}
      let wxs = ConT (mkName "Widget") `AppT` VarT xsName
      vars <- replicateM (length $ opr^.args) $ newName "x"

      return $
        [ SigD opName
          $ ForallT [] [(ConT (mkName "∈")) `AppT` ConT (opr^.name) `AppT` VarT xsName]
          $ buildArr (opr^.args)
          $ ConT (mkName "Getter")
          `AppT` wxs
          `AppT` (if isIdentity then id else AppT (opr^.monad)) (opr^.rettype)
        , FunD opName
          [ Clause
            (fmap VarP vars)
            (NormalB $ VarE (mkName $ if isIdentity then "_value'" else "_value") `AppE` foldl (\e n -> e `AppE` VarE n) (ConE (opr^.name)) vars)
            []
          ]
        ]

parseWithWild :: Name -> Type -> Operator
parseWithWild n = getArgs getOpTypes
  where
    getArgs :: (Type -> Operator) -> Type -> Operator
    getArgs k (AppT (AppT ArrowT a) b) = getArgs k b & args %~ (a :)
    getArgs k u = k u

    getOpTypes :: Type -> Operator
    getOpTypes (AppT (AppT (AppT WildCardT br) m) r) = Operator [] n br m r

-- | Build a operator datatype and its getter function
--
-- @
-- makeOp "Piyo1" [t| A -> B -> _ Self m a |]
-- makeOp "Piyo2" [t| A -> B -> _ Value Identity a |]
-- makeOp "Piyo3" [t| A -> B -> _ t m a |]
-- @
--
-- will generate
--
-- @
-- data Op'Piyo1 br m r where
--   Op'Piyo1 :: A -> B -> Op'Piyo1 Self m a
-- op'piyo1 :: Op'Piyo1 ∈ xs => A -> B -> Getter (Widget xs) (m (Widget xs))
--
-- data Op'Piyo2 br m r where
--   Op'Piyo2 :: A -> B -> Op'Piyo2 Value Identity a
-- op'piyo2 :: Op'Piyo2 ∈ xs => A -> B -> Getter (Widget xs) a
--
-- data Op'Piyo3 br m r where
--   Op'Piyo3 :: A -> B -> Op'Piyo3 t m a
-- op'piyo1 :: Op'Piyo3 ∈ xs => A -> B -> Getter (Widget xs) (FreezeT (Widget xs) m ())
-- @
makeOp :: String -> TypeQ -> DecsQ
makeOp name typQ = typQ >>= \typ -> do
  let n = mkName $ "Op'" ++ name
  let opr = parseWithWild n typ
  fmap concat $ sequence [declareOpDatatype opr, declareOpGetter opr]

