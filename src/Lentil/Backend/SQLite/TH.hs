{-# LANGUAGE TemplateHaskell #-}

module Lentil.Backend.SQLite.TH where

-- TODO: Add Template Haskell approach
-- Converts the description of the datatype to the entity descriptionn

import Language.Haskell.TH
import Lentil.Backend.SQLite.Core
import Lentil.Core
import Lentil.SQL.TH

{-
Template Haskell solution:

Generate SQL string CREATE TABLE
1. An ADT has one constructor
   Generates a string that creates a table for every field.
2. An ADT has more than one constructor
   Generates multiple tables and a connector table with foreign key,
   or a unified table with every of the fields.

Generate SQL string SELECTS TABLE for the entity
1. An ADT has one constructor
   Generates a SQL command that selects all the field
2. An ADT has more than one constructor
   Generates a SQL command that selects a constructor and uses

The SQL strings can be used to generate the default entity handlers for
the entities.
-}

infixl 0 $$

($$) :: Type -> Type -> Type
t1 $$ t2 = AppT t1 t2

t :: Name -> Type
t = ConT

makeEntity :: Name -> Q [Dec]
makeEntity name = do
  sql <- sqlCommands' name
  let typeName = sqlTypeName sql
  entityBody <- [| entityLens $ Entity { loadEntity = undefined, updateEntity = undefined } |]
  let entitySig = SigD (mkName typeName)
                       ((t ''EntityLens') $$ (t ''SQLite) $$ ((t ''ID) $$ (t name)) $$ (t name))
      entityDef = ValD (VarP (mkName typeName)) (NormalB entityBody) []
  pure
    [ sqlCreate sql
    , sqlDrop sql
    , sqlInsert sql
    , sqlSelect sql
    , sqlUpdate sql
    , sqlDelete sql
    , entitySig
    , entityDef
    ]

{-
loadEntry :: ID Entry -> SQLite Entry
loadEntry pk = do
  conn <- sqliteConn
  r <- sqliteIO $ queryNamed
        conn "SELECT note, start, end, sheet FROM entries WHERE id=:eid"
             [":eid" := _unID pk]
  case r of
    [r@(n,st,e,sh)] -> pure $ fromTuple r
    _               -> error "loadEntry returned not one line."

updateEntry :: ID Entry -> Entry -> SQLite ()
updateEntry pk (Entry n st e sh) = do
  conn <- sqliteConn
  sqliteIO $ executeNamed conn
    "UPDATE entries SET note=:n, start=:st, end=:e, sheet=:sh WHERE id=:eid"
    [":n" := n, ":st" := st, ":e" := e, ":sh" := sh, ":eid" := _unID pk]

entry :: EntityLens' SQLite (ID Entry) Entry
entry = entityLens $ Entity { loadEntity = loadEntry, updateEntity = updateEntry }
-}

{-
curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where
    mkCurryDec ith = do
      cury <- curryN ith
      let name = mkName $ "curry" ++ show ith
      return $ FunD name [Clause [] (NormalB cury) []]
-}
