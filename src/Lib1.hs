{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib1
  ( parseSelectAllStatement,
    findTableByName,
    validateDataFrame,
    renderDataFrameAsTable,
  )
where

import DataFrame (DataFrame (..), Row, Column (..), ColumnType (..), Value (..))
import InMemoryTables (TableName)
import Data.Char (toLower, toUpper)

type ErrorMessage = String

type Database = [(TableName, DataFrame)]

-- Your code modifications go below this comment

-- 1) implement the function which returns a data frame by its name
-- in provided Database list
findTableByName :: Database -> String -> Maybe DataFrame
findTableByName db tableName = lookup (map toLower tableName) (map (\(n, df) -> (map toLower n, df)) db)

-- 2) implement the function which parses a "select * from ..."
-- sql statement and extracts a table name from the statement
parseSelectAllStatement :: String -> Either ErrorMessage TableName
parseSelectAllStatement stmt =
  let trimmedStmt = removeTrailingSemicolon $ stmt
      stmtWords = words $ map toUpper trimmedStmt
   in case stmtWords of
        ["SELECT", "*", "FROM", tableName] -> Right (map toLower tableName)
        _ -> Left "Invalid SQL statement format"
  where
    removeTrailingSemicolon :: String -> String
    removeTrailingSemicolon str = if not (null str) && last str == ';' then init str else str

-- 3) implement the function which validates tables: checks if
-- columns match value types, if rows sizes match columns,..
validateDataFrame :: DataFrame -> Either ErrorMessage ()
validateDataFrame (DataFrame cols rows) = do
  validateRowLength
  validateRowTypes
  where
    validateRowLength =
      if all (\row -> length row == length cols) rows
        then Right ()
        else Left "Row length does not match number of columns"

    validateRowTypes =
      if all isValidRow rows
        then Right ()
        else Left "Data types do not match column types"

    isValidRow row = all isValidValue (zip cols row)

    isValidValue (Column _ colType, value) = colType == typeOf value || isNullValue value

    isNullValue :: Value -> Bool
    isNullValue NullValue = True
    isNullValue _ = False

    typeOf IntegerValue {} = IntegerType
    typeOf StringValue {} = StringType
    typeOf BoolValue {} = BoolType
    typeOf NullValue = IntegerType -- This is not used when isNullValue is True

-- 4) implement the function which renders a given data frame
-- as ascii-art table (use your imagination, there is no "correct"
-- answer for this task!), it should respect terminal
-- width (in chars, provided as the first argument)
renderDataFrameAsTable :: Integer -> DataFrame -> String
renderDataFrameAsTable _ (DataFrame cols rows) =
  unlines $
    [unwords $ map show cols]
      ++ map (unwords . map show) rows
