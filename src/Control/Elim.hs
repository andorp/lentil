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
  TyConI (DataD _ tname _ constructors _) <- reify t
  let consElimPat (NormalC name _) = VarP . mkName . map toLower $ nameBase name

      consPatterns = consElimPat <$> constructors

      elimClause (NormalC name fields) = do
        let constructorName = mkName . map toLower $ nameBase name
        (pats, vars) <- genPE (length fields)
        return $ Clause (consPatterns ++ [ConP name pats])
                        (NormalB (foldl' AppE (VarE constructorName) vars))
                        []

  elimBody <- mapM elimClause constructors
  let elimName = mkName $ (toLower <$> nameBase tname) ++ "Elim"
  return [FunD elimName elimBody]

  where
    genPE n = do
      ids <- replicateM n (newName "x")
      return (VarP <$> ids, VarE <$> ids)
