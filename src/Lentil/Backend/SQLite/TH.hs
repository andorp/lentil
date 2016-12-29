{-# LANGUAGE TemplateHaskell #-}

module Lentil.Backend.SQLite.TH where

-- TODO: Add Template Haskell approach
-- Converts the description of the datatype to the entity descriptionn

import Control.Monad
import Data.List (foldl')
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Elim
import Lentil.Backend.SQLite.Core
import Lentil.Core
import Lentil.SQL.TH
import Lentil.Types

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
