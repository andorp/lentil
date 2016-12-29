{-# LANGUAGE TemplateHaskell #-}
module Control.Elim where

import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')

import Language.Haskell.TH

-- | Creates the Generic Eliminator for the given ADT.
--
-- data Test = Test1 | Test2 Int | Test3 String
--
-- makeElim ''Test ~
--
-- testElim test1 test2 test3 Test1     = test1
-- testElim test1 test2 test3 (Test2 x) = test2 x
-- testElim test1 test2 test3 (Test3 x) = test3 x
--
makeElim :: Name -> Q [Dec]
makeElim t = do
  tyCon <- reify t
  let (tname, constructors) =
        case tyCon of
          TyConI (DataD _ tname _ constructors _)   -> (tname, constructors)
          TyConI (NewtypeD _ tname _ constructor _) -> (tname, [constructor])

  let consName (NormalC n _)   = n
      consName (RecC n _)      = n
      consName (InfixC _ n _)  = n
      consName (ForallC _ _ c) = consName c

      consElimPat = VarP . mkName . deCapitalize . nameBase . consName

      consPatterns = consElimPat <$> constructors

      elimClause (NormalC name fields) = do
        let constructorName = mkName . deCapitalize $ nameBase name
        (pats, vars) <- genPE (length fields)
        return $ Clause (consPatterns ++ [ConP name pats])
                        (NormalB (foldl' AppE (VarE constructorName) vars))
                        []

  elimBody <- mapM elimClause constructors
  let elimName = mkName . deCapitalize $ nameBase tname ++ "Elim"
  return [FunD elimName elimBody]

  where
    deCapitalize (x:xs) = toLower x : xs
    genPE n = do
      ids <- replicateM n (newName "x")
      return (VarP <$> ids, VarE <$> ids)
