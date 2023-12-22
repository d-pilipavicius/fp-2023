{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SQLCustomDataTypes where

import DataFrame

type ErrorMessage = String 
type Database = [(String, DataFrame)]

data ParsedStatement = ParsedTable {
  table :: String,
  columns :: Columns,
  rowConditions :: Conditions,
  orderBy :: Order
} | ParsedTables {
  tables :: Tables
} | ManyTables {
  tNames :: [String],
  columns :: Columns,
  rowConditions :: Conditions,
  orderBy :: Order
} | UpdateTable {
  table :: String,
  tableChanges :: Asigning,
  rowConditions :: Conditions
} | DeleteTableElements {
  table :: String,
  rowConditions :: Conditions
} | InsertInto {
  table :: String,
  columns :: Columns,
  insertedValues :: [[String]]
} | CreateTable {
  table :: String,
  columnData :: [Column]
} | DropTable {
  table :: String
} deriving (Eq, Show)

data Tables = AllT | Table String | NullT deriving (Eq, Show)

data Columns = AllC | ColumnList [Columns] | Func {name :: String, column :: Columns} | OneColumn String | NullC deriving (Eq, Show)

data Conditions = NoConditions | Conditions [(String, String, String)] deriving (Eq, Show)

data Asigning = ValueChanges [Asigning] | Asign {asgCol :: Columns, asgValue :: String} deriving (Eq, Show) 

data Order = OrderList [Order] | OrderOne String Bool | NullOr deriving (Eq, Show) --OrderOne Bool: True - ASC, False - DESC

data LogOp = LogOp ConData String ConData

data ConData = ColumnName String | ConData Value deriving (Eq, Show)
