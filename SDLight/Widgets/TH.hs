module SDLight.Widgets.TH
  ( makeOp
  ) where

import Control.Lens
import Data.Char
import Language.Haskell.TH

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
declareOpGetter opr = do
  xsName <- newName "xs"
  let opName = mkName $ (\s -> let (a,b) = splitAt 4 s in fmap toLower a ++ b) $ show $ opr^.name

  {- op'piyo :: Op'Piyo ∈ xs => A -> B -> Getter (Widget xs) (FreezeT (Widget xs) Identity ())
     op'piyo = _Op Op'Piyo -}
  let wxs = ConT (mkName "Widget") `AppT` VarT xsName
  return $
    [ SigD opName
      $ ForallT [] [(ConT (mkName "∈")) `AppT` ConT (opr^.name) `AppT` VarT xsName]
      $ buildArr (opr^.args)
      $ ConT (mkName "Getter")
      `AppT` wxs
      `AppT` ((opr^.branch) `AppT` wxs `AppT` (opr^.monad) `AppT` (opr^.rettype))
    , FunD opName
      [ Clause []
        (NormalB $ VarE (mkName "_Op") `AppE` ConE (opr^.name))
        []
      ]
    ]

parseWithWild :: Name -> Type -> Operator
parseWithWild n = getArgs getOpTypes
  where
    getArgs :: (Type -> Operator) -> Type -> Operator
    getArgs k (AppT (AppT ArrowT a) b) = k b & args %~ (a :)
    getArgs k u = k u

    getOpTypes :: Type -> Operator
    getOpTypes (AppT (AppT (AppT WildCardT br) m) r) = Operator [] n br m r

makeOp :: String -> TypeQ -> DecsQ
makeOp name typQ = typQ >>= \typ -> do
  let n = mkName $ "Op'" ++ name
  let opr = parseWithWild n typ
  fmap concat $ sequence [declareOpDatatype opr, declareOpGetter opr]
 
