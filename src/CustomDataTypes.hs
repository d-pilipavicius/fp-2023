module CustomDataTypes where

import DataFrame

type ErrorMessage = String 

data ParsedStatement = ParsedTable {
  table :: String,
  columns :: Columns,
  rowConditions :: Conditions
} | ParsedTables {
  tables :: Tables
} deriving (Eq, Show)

data Tables = AllT | Table String | NullT deriving (Eq, Show)

data Columns = AllC | ColumnList [String] | Func {name :: String, column :: Columns} | OneColumn String | NullC deriving (Eq, Show)

data Conditions = NoConditions | Conditions [(String, String, String)] deriving (Eq, Show)

data LogOp = LogOp ConData String ConData

data ConData = ColumnName String | ConData Value deriving (Eq, Show)
