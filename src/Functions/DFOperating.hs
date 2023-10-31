{-# LANGUAGE InstanceSigs #-}
module Functions.DFOperating (
    isColumn,
    removeRow,
    removeColumn,
    removeColumnsByName,
    getColumnWhole,
    getColumnData,
    columnsInTable,
    getColumnRows,
    getRow,
    getColumnId,
    getRowValue,
    maxInColumn,
    sumUpRows
) where

import Errors
import DataFrame
import CustomDataTypes
import Functions.ListOperating

instance Ord Value where
  (>) :: Value -> Value -> Bool
  IntegerValue a > IntegerValue b = a > b
  StringValue a > StringValue b = a > b
  BoolValue a > BoolValue b = a > b
  NullValue > NullValue = False
  IntegerValue _ > NullValue = True
  BoolValue _ > NullValue = True
  StringValue _ > NullValue = True
  NullValue > StringValue _ = False
  NullValue > IntegerValue _ = False
  NullValue > BoolValue _ = False

  (<) :: Value -> Value -> Bool
  x < y = y > x

isColumn :: String -> DataFrame -> Bool
isColumn s (DataFrame c _) = isColumn1 s c

removeRow :: Integer -> DataFrame -> DataFrame
removeRow i (DataFrame cols rows) = DataFrame cols $ removeElementFromList i [] rows

removeColumn :: Integer -> DataFrame -> DataFrame
removeColumn i (DataFrame cols rows) = DataFrame (removeElementFromList i [] cols) (removeColumnFromList i [] rows)

removeColumnsByName :: [String] -> DataFrame -> DataFrame
removeColumnsByName s df =
  case cropDuplicates s of
    s -> removeColumnsByName1 s df

getColumnWhole :: String -> DataFrame -> Either ErrorMessage DataFrame
getColumnWhole s df = 
  case getColumnData s df of 
    Right r -> Right $ DataFrame [r] (getColumnRows s df)
    Left l -> Left l

getColumnData :: String -> DataFrame -> Either ErrorMessage Column
getColumnData name (DataFrame [] _) = Left $ _ERROR_COLUMN_NOT_FOUND name
getColumnData name (DataFrame ((Column x t):xs) _) = if x == name 
  then Right (Column x t) 
  else getColumnData name (DataFrame xs [])

columnsInTable :: [String] -> DataFrame -> Either ErrorMessage DataFrame
columnsInTable [] df = Right df
columnsInTable (s:ss) (DataFrame cols _) = if elem s $ map (\(Column name _)->name) cols
  then columnsInTable ss (DataFrame cols [])
  else Left $ _ERROR_COLUMN_NOT_FOUND s

getColumnRows :: String -> DataFrame -> [Row]
getColumnRows s (DataFrame [] _) = []
getColumnRows s df = 
  case df of
    DataFrame ((Column name _):xs) r -> if name == s 
      then map (\(x:xs) -> [x]) r
      else getColumnRows s (removeColumn 0 df)

getRow :: Integer -> DataFrame -> Row
getRow 0 (DataFrame _ []) = []
getRow 0 (DataFrame _ (x:_)) = x 
getRow i (DataFrame _ (_:xs)) = getRow (i-1) (DataFrame [] xs)

getColumnId :: String -> DataFrame -> Integer
getColumnId s df = getColumnId1 s df 0 

getRowValue :: Integer -> Row -> Value
getRowValue 0 (x:_) = x
getRowValue _ [] = NullValue
getRowValue i (_:xs) = getRowValue (i-1) xs

maxInColumn :: String -> DataFrame -> DataFrame
maxInColumn cName df =
  case getColumnRows cName df of
    (x:xs) -> case df of
      (DataFrame [Column _ t] _) -> DataFrame [Column ("max_"++cName) t] [[maxValueInRows xs x]]

sumUpRows :: [Row] -> Row
sumUpRows r = sumUpRows1 r 0

getColumnId1 :: String -> DataFrame -> Integer -> Integer
getColumnId1 _ (DataFrame [] _) _ = -1
getColumnId1 s (DataFrame ((Column cName _):xs) _) i = if s == cName
  then i
  else getColumnId1 s (DataFrame xs []) (i+1)

isColumn1 :: String -> [Column] -> Bool
isColumn1 _ [] = False
isColumn1 s ((Column n _):xs) = if n == s then True else isColumn1 s xs

sumUpRows1 :: [Row] -> Integer -> Row
sumUpRows1 [] i = [IntegerValue i]
sumUpRows1 ([IntegerValue nr]:xs) i = sumUpRows1 xs (i+nr)

removeColumnsByName1 :: [String] -> DataFrame -> DataFrame
removeColumnsByName1 [] df = df
removeColumnsByName1 (x:xs) df = removeColumnsByName xs (removeColumnByName x df)

removeColumnByName :: String -> DataFrame -> DataFrame
removeColumnByName s df = removeColumn (getColumnId s df) df

maxValueInRows :: [Row] -> Row -> Value
maxValueInRows [] [val] = val
maxValueInRows ([val1]:xs) [val2] = if val1 > val2
  then maxValueInRows xs [val1]
  else maxValueInRows xs [val2]
