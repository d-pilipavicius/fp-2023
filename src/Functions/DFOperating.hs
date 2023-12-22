{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Functions.DFOperating (
    isColumn,
    areColumns,
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
    sumUpRows,
    addTableNamePrefix,
    changeColumnValuesTo,
    changeAllColumnValuesTo,
    getRightTableSide,
    getLeftTableSide,
    multiplyTwoTables,
    multiplyMultipleTables,
    connectTwoTables,
    removeRowsFromDF,
    parseIntegerValue,
    parseBoolValue,
    parseNullValue,
    parseStringValue,
    dfRowCount,
    dfColCount,
    dfCol,
    dfRows,
    colName,
    findTableByNameNoCase
) where

import Errors ( _ERROR_COLUMN_NOT_FOUND, _ERROR_TABLE_ROW_NOT_MATCHING )
import DataFrame ( DataFrame(..), Column(..), Row, Value(..) )
import CustomDataTypes ( ErrorMessage )
import Functions.ListOperating
import Functions.ValueComparing (stringToInt, stringToLower)

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

  (>=) :: Value -> Value -> Bool
  x >= y = x > y || x == y

  (<=) :: Value -> Value -> Bool
  x <= y = y > x || x == y

isColumn :: DataFrame -> String -> Bool
isColumn (DataFrame c _) s = isColumn1 s c

areColumns :: DataFrame -> [String] -> Either ErrorMessage ()
areColumns _ [] = Right ()
areColumns df (x:xs) = 
  if isColumn df x
    then areColumns df xs 
    else Left $ _ERROR_COLUMN_NOT_FOUND x

removeRow :: Integer -> DataFrame -> DataFrame
removeRow i (DataFrame cols rows) = DataFrame cols $ removeElementFromList i [] rows

removeColumn :: Integer -> DataFrame -> DataFrame
removeColumn i (DataFrame cols rows) = DataFrame (removeElementFromList i [] cols) (removeColumnFromList i [] rows)

removeColumnsByName :: [String] -> DataFrame -> DataFrame
removeColumnsByName s df =
  case cropDuplicates s of
    s1 -> removeColumnsByName1 s1 df

getColumnWhole :: String -> DataFrame -> Either ErrorMessage DataFrame
getColumnWhole "" _ = Right $ DataFrame [] []
getColumnWhole s df =
  case getColumnData s df of
    Right r -> Right $ DataFrame [r] (getColumnRows s df)
    Left l -> Left l

getColumnData :: String -> DataFrame -> Either ErrorMessage Column
getColumnData cName (DataFrame [] _) = Left $ _ERROR_COLUMN_NOT_FOUND cName
getColumnData cName (DataFrame ((Column x t):xs) _) = if x == cName
  then Right (Column x t)
  else getColumnData cName (DataFrame xs [])

columnsInTable :: [String] -> DataFrame -> Either ErrorMessage DataFrame
columnsInTable [] df = Right df
columnsInTable (s:ss) (DataFrame cols _) = if elem s $ map (\(Column cName _)-> cName) cols
  then columnsInTable ss (DataFrame cols [])
  else Left $ _ERROR_COLUMN_NOT_FOUND s

getColumnRows :: String -> DataFrame -> [Row]
getColumnRows _ (DataFrame [] _) = []
getColumnRows s df =
  case df of
    DataFrame ((Column cName _):_) r -> if cName == s
      then map (\(x:_) -> [x]) r
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
isColumn1 s ((Column n _):xs) = (n == s) || isColumn1 s xs

sumUpRows1 :: [Row] -> Integer -> Row
sumUpRows1 [] i = [IntegerValue i]
sumUpRows1 ([IntegerValue nr]:xs) i = sumUpRows1 xs (i+nr)
sumUpRows1 ([NullValue]:xs) i = sumUpRows1 xs i

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

connectRows :: [Row] -> [Row] -> [Row]
connectRows r1 r2 = connectRows1 r1 r2 []

connectRows1 :: [Row] -> [Row] -> [Row] -> [Row]
connectRows1 [] _ r = r
connectRows1 (xr1:xr1s) (xr2:xr2s) r3 = connectRows1 xr1s xr2s (r3++[xr1++xr2])

dfCol :: DataFrame -> [Column]
dfCol (DataFrame c _) = c

dfRows :: DataFrame -> [Row]
dfRows (DataFrame _ r) = r

dfColCount :: DataFrame -> Integer
dfColCount = toInteger . length . dfCol

dfRowCount :: DataFrame -> Integer
dfRowCount = toInteger . length . dfRows    

connectTwoTables :: DataFrame -> DataFrame -> Either ErrorMessage DataFrame
connectTwoTables df1 df2 = do
  let rows1 = dfRows df1
  let rows2 = dfRows df2
  let col1 = dfCol df1
  let col2 = dfCol df2
  if dfRowCount df1 == dfRowCount df2 
    then return $ DataFrame (col1++col2) (connectRows rows1 rows2)
    else if dfRowCount df1 == 0 && dfColCount df1 == 0 || dfRowCount df2 == 0 && dfColCount df2 == 0
      then return $ DataFrame (col1++col2) (rows1++rows2)
      else Left _ERROR_TABLE_ROW_NOT_MATCHING

multiplyTwoTables :: DataFrame -> DataFrame -> DataFrame
multiplyTwoTables df1 df2 = do
  let rows1 = dfRows df1
  let rows2 = dfRows df2
  let col1 = dfCol df1
  let col2 = dfCol df2
  DataFrame (col1++col2) [a++b | a <- rows1, b <- rows2]

getLeftTableSide :: DataFrame -> String -> Either ErrorMessage DataFrame
getLeftTableSide df cName =
  if isColumn df cName
    then Right $ getLeftTableSide1 df (dfColCount df - getColumnId cName df - 1)
    else Left $ _ERROR_COLUMN_NOT_FOUND cName

getLeftTableSide1 :: DataFrame -> Integer -> DataFrame
getLeftTableSide1 df 0 = removeColumn (dfColCount df - 1) df
getLeftTableSide1 df i = do
  let newDf = removeColumn (dfColCount df - 1) df
  getLeftTableSide1 newDf $ i-1

getRightTableSide :: DataFrame -> String -> Either ErrorMessage DataFrame
getRightTableSide df cName =
  if isColumn df cName
    then Right $ getRightTableSide1 df (getColumnId cName df)
    else Left $ _ERROR_COLUMN_NOT_FOUND cName

getRightTableSide1 :: DataFrame -> Integer -> DataFrame
getRightTableSide1 df 0 = removeColumn 0 df
getRightTableSide1 df i = do
  let newDf = removeColumn 0 df
  getRightTableSide1 newDf $ i-1

addTableNamePrefix :: DataFrame -> String -> DataFrame
addTableNamePrefix df tName = do
  let cols = dfCol df
  let rows = dfRows df
  let cols1 = fmap (\(Column cName cType) -> Column (tName++"."++cName) cType) cols
  DataFrame cols1 rows

changeColumnValuesTo :: String -> Value -> Value -> DataFrame -> Either ErrorMessage DataFrame
changeColumnValuesTo col from to df = do
  lSide <- getLeftTableSide df col
  rSide <- getRightTableSide df col
  let cRows = getColumnRows col df
  let mRows = fmap (\[x] -> if x == from then [to] else [x]) cRows
  colData <- getColumnData col df 
  let cDf = DataFrame [colData] mRows
  connectTwoTables lSide cDf >>= connectTwoTables rSide

changeAllColumnValuesTo :: Value -> Value -> DataFrame -> Either ErrorMessage DataFrame
changeAllColumnValuesTo from to df = do
  let cols = map (\(Column cName _) -> cName) $ dfCol df
  changeAllColumnValuesTo1 cols from to df 

changeAllColumnValuesTo1 :: [String] -> Value -> Value -> DataFrame -> Either ErrorMessage DataFrame
changeAllColumnValuesTo1 [] _ _ df = Right df
changeAllColumnValuesTo1 (x:xs) from to df = do
  newDf <- changeColumnValuesTo x from to df
  changeAllColumnValuesTo1 xs from to newDf 

multiplyMultipleTables :: [DataFrame] -> DataFrame
multiplyMultipleTables (x:xs) = multiplyMultipleTables1 xs x 

multiplyMultipleTables1 :: [DataFrame] -> DataFrame -> DataFrame
multiplyMultipleTables1 [] df = df
multiplyMultipleTables1 (x:xs) df = multiplyMultipleTables1 xs $ multiplyTwoTables df x 

removeRowsFromDF :: DataFrame -> [Row] -> DataFrame
removeRowsFromDF (DataFrame cols rows) rows1 = DataFrame cols $ listSubtraction rows rows1

parseIntegerValue :: String -> Either ErrorMessage Value
parseIntegerValue s = 
  case stringToInt s of
    Right r -> Right $ IntegerValue r
    Left _ -> Left "Not an Integer."

parseBoolValue :: String -> Either ErrorMessage Value
parseBoolValue s = 
  case stringToLower s of
    "true" -> Right $ BoolValue True
    "false" -> Right $ BoolValue False
    _ -> Left "Not a Bool."

parseNullValue :: String -> Either ErrorMessage Value
parseNullValue s =
  case stringToLower s of
    "null" -> Right NullValue
    _ -> Left "Not null."

parseStringValue :: String -> Either ErrorMessage Value
parseStringValue s =
  case s of
    ('\'':xs) -> do
      let str = takeWhile (/= '\'') xs
      if drop (length str+1) s == "\'"
        then return $ StringValue str
        else Left "Not a String."
    _ -> Left "Not a String."

colName :: Column -> String
colName (Column n _) = n

findTableByNameNoCase :: [(String, DataFrame)] -> String -> Maybe DataFrame
findTableByNameNoCase [] _ = Nothing
findTableByNameNoCase _ "" = Nothing
findTableByNameNoCase (x:xs) n = 
	if stringToLower (fst x) == n 
    then Just $ snd x 
    else findTableByNameNoCase xs n