{-# LANGUAGE TemplateHaskell #-}
module Lentil.SQL.TH where

import Data.Char (toLower, toUpper)
import Data.List (intercalate, sort)
import Language.Haskell.TH



data SQLCommands = SQLCommands
  { sqlTypeName  :: String
  , sqlCreate :: Dec
  , sqlDrop   :: Dec
  , sqlInsert :: Dec
  , sqlSelect :: Dec
  , sqlUpdate :: Dec
  , sqlDelete :: Dec
  }

-- | Creates SQL strings which are CREATE TABLE, SELECT, UPDATE and DELETE TABLE.
-- The name of the table will be the name of the type. The commands will be
-- the name of the type and Command
--
-- FIXME: Generated _select
-- FIXME: Generate type signatures with meaningful names
sqlCommands' :: Name -> Q SQLCommands
sqlCommands' t = do
  tyCon <- reify t
  let (tname, [constructor]) =
        case tyCon of
          TyConI (DataD _ tname' _ _ constructors' _)   -> (tname', constructors')
          TyConI (NewtypeD _ tname' _ _ constructor' _) -> (tname', [constructor'])
          bad -> error $ "sqlCommands: Not supported type constructors" ++ show bad

  let RecC _cname fields = constructor
      typeName    = deCapitalize $ nameBase tname
      tableName   = typeName
      typeNameF s = mkName $ typeName ++ s

      sortedFields = sort fields
      sortedFieldNames = (\(name, _, _) -> nameBase name) <$> sortedFields
      createFields     = (\(name, _, type_) -> unwords[nameBase name, sqlType type_]) <$> sortedFields
      updateFields     = (\(name, _, _) -> concat [nameBase name,"=:",nameBase name]) <$> sortedFields

      insertNamePair n = ( mkName $ "toSql" ++ (capitalize $ nameBase n)
                         , mkName $ nameBase n
                         )
      insertValues     = listE $ (\(name, _, _) ->
                                    let (sn, n) = insertNamePair name
                                    in (appE (varE sn) (varE n))
                                 ) <$> sortedFields
      insertParams     = concatMap
                            (\(name, _, _) -> let (sn, n) = insertNamePair name
                                              in [ VarP sn, VarP n ])
                            fields

      idVal = mkName "idVal"
      toSql = mkName "toSql"
  createTable <- [| unwords ["CREATE", "TABLE", tableName, "(", "id INTEGER PRIMARY KEY ASC,", intercalate ", " createFields ,")", ";"] |]
  dropTable   <- [| unwords ["DROP", "TABLE", tableName, ";"] |]
  insertRow <- [| unwords ["INSERT", "INTO", tableName, "(", intercalate ", " sortedFieldNames, ")", "VALUES", "(", $(appE [| intercalate ", " |] insertValues), ")", ";"] |]
  selectRow <- [| unwords ["SELECT", (intercalate ", " sortedFieldNames), "FROM", tableName, "WHERE id=", $(appE (varE toSql) (varE idVal)), ";"] |]
  updateRow <- [| unwords ["UPDATE", tableName, "SET", intercalate ", " updateFields, "WHERE", "id=", $(appE (varE toSql) (varE idVal)), ";"] |]
  deleteRow <- [| unwords ["DELETE", "FROM", tableName, "WHERE", "id=", $(appE (varE toSql) (varE idVal)), ";"] |]
  pure $ SQLCommands
    { sqlTypeName = typeName
    , sqlCreate = ValD (VarP $ typeNameF "Create") (NormalB createTable) []
    , sqlDrop   = ValD (VarP $ typeNameF "Drop")   (NormalB dropTable)   []
    , sqlInsert = FunD (typeNameF "Insert") [Clause insertParams (NormalB insertRow) []]
    , sqlSelect = FunD (typeNameF "Select") [Clause [VarP toSql, VarP idVal] (NormalB selectRow) []]
    , sqlUpdate = FunD (typeNameF "Update") [Clause [VarP toSql, VarP idVal] (NormalB updateRow) []]
    , sqlDelete = FunD (typeNameF "Delete") [Clause [VarP toSql, VarP idVal] (NormalB deleteRow) []]
    }
  where
    capitalize   []     = error "capitalize: got empty name"
    capitalize   (x:xs) = toUpper x : xs
    deCapitalize []     = error "deCapitalize: got empty name"
    deCapitalize (x:xs) = toLower x : xs

sqlCommands :: Name -> Q [Dec]
sqlCommands name = do
  s <- sqlCommands' name
  pure
    [ sqlCreate s
    , sqlDrop s
    , sqlInsert s
    , sqlSelect s
    , sqlUpdate s
    , sqlDelete s
    ]

sqlType :: Type -> String
sqlType t@(ConT name) = case nameBase name of
  "Int"        -> "INTEGER"
  "Double"     -> "FLOAT"
  "String"     -> "TEXT"
  "ByteString" -> "BLOB"
  _            -> error $ "Unsupported sql row primitive type: " ++ show t
sqlType t = error $ "Unsupported sql row type: " ++ show t
