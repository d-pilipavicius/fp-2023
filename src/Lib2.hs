{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib2( 
    parseStatement,
    executeStatement
  )
where

import Errors
import Parser
import Executor
import CustomDataTypes
import Functions.DFOperating
import Functions.ListOperating
import Functions.ValueComparing
import Lib1 (findTableByName)
import InMemoryTables (TableName, database)
import DataFrame (DataFrame (..), Column (..), ColumnType (..), Value (..))
import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isSpace)

type Database = [(TableName, DataFrame)]
type ValidOperators = [String]

--Constants
errorTemplate :: Either ErrorMessage (String, a) -> Parser a
errorTemplate a = Parser $ \_ -> a

semicolonError :: Parser a
semicolonError = errorTemplate $ Left _ERROR_MISSING_SEMICOLON

noTablesError :: Parser a
noTablesError = errorTemplate $ Left _ERROR_NO_TABLES_AFTER_SHOW

badFirstCommandError :: Parser a
badFirstCommandError = errorTemplate $ Left _ERROR_BAD_FIRST_COMMAND

unrecognisedCommandError :: Parser a
unrecognisedCommandError = errorTemplate $ Left _ERROR_UNRECOGNISED_COMMAND_AFTER_FROM

noColumnsError :: Parser a
noColumnsError = errorTemplate $ Left _ERROR_NO_COLUMNS_AFTER_SELECT

tableMissingError :: Parser a
tableMissingError = errorTemplate $ Left _ERROR_NO_TABLE_CLAUSE

unrecognisedSymbolsAfterStatementError :: Parser a
unrecognisedSymbolsAfterStatementError = errorTemplate $ Left _ERROR_UNRECOGNISED_SYMBOLS
--End of constants

--TASK
parseStatement :: String -> Either ErrorMessage ParsedStatement
parseStatement query =
  case runParser parseWholeStatement query of
    Left e -> Left e
    Right ("", s) -> Right s
    Right (xs, s) -> Left _ERROR_CHARACTERS_AFTER_SEMICOLON

executeStatement :: ParsedStatement -> Either ErrorMessage DataFrame
executeStatement st = case st of
  ParsedTable {} -> executeSelect st database
  ParsedTables {} -> executeShow st database
--TASK END

-----------------------------------------------------------------PARSER SETUP
--Complete parse
parseWholeStatement :: Parser ParsedStatement
parseWholeStatement = do
  typeS <- parseStatementType <|> badFirstCommandError
  case typeS of
    ParsedTables {} -> parseRestShow
    ParsedTable {} -> parseRestSelect
--Complete parse END

--Initial word
parseStatementType :: Parser ParsedStatement
parseStatementType = parseShow <|> parseSelect
--Initial word END

--Constant names parsers
parseSelect :: Parser ParsedStatement
parseSelect = (\_ -> ParsedTable "" NullC NoConditions) <$> parseInsensitiveWord "SELECT"

parseShow :: Parser ParsedStatement
parseShow = (\_ -> ParsedTables NullT) <$> parseInsensitiveWord "SHOW"
--Constant names parsers END

--Table names
parseRestShow :: Parser ParsedStatement
parseRestShow = do
  _ <- many parseSpace
  stat <- parseShowTableNames
  _ <- parseEnding
  return stat

parseShowTableNames :: Parser ParsedStatement
parseShowTableNames = fmap ParsedTables (parseAllT <|> parseTable) <|> noTablesError

parseAllT :: Parser Tables
parseAllT = (\_ -> AllT) <$> parseInsensitiveWord "TABLES"

parseTable :: Parser Tables
parseTable = do
  _ <- parseInsensitiveWord "table" <|> tableMissingError
  _ <- many parseSpace
  fmap Table parseName
--Table names END

--Select statement setup
parseRestSelect :: Parser ParsedStatement
parseRestSelect = do
  _ <- many parseSpace
  pColumns <- parseColumns
  _ <- many parseSpace
  _ <- parseInsensitiveWord "FROM"
  _ <- many parseSpace
  tableName <- parseName
  _ <- many parseSpace
  ParsedTable tableName pColumns <$> parseConditions

--Select statement setup END

--Column names
parseColumns :: Parser Columns
parseColumns = (parseAllC <|> parseAggregate <|> parseColumnList) <|> noColumnsError

parseAllC :: Parser Columns
parseAllC = (\_ -> AllC) <$> parseChar '*'

parseSingleC :: Parser Columns
parseSingleC = fmap OneColumn parseName

parseAggregate :: Parser Columns
parseAggregate = do
  fun <- parseName
  _ <- many parseSpace
  _ <- parseChar '('
  _ <- many parseSpace
  pColumn <- parseAllC <|> parseSingleC
  _ <- many parseSpace
  _ <- parseChar ')'
  return $ Func {name = (stringToLower fun), column = pColumn}

parseColumnList :: Parser Columns
parseColumnList = do
  pName <- parseName
  other <- many parseNames
  return $ ColumnList $ pName:other

parseNames :: Parser String
parseNames = do
    _ <- many parseSpace
    _ <- parseChar ','
    _ <- many parseSpace
    parseName
--Column names END

--Where statement check
parseConditions :: Parser Conditions
parseConditions = parseWhereEnding <|> parseWhere

parseWhere :: Parser Conditions
parseWhere = do
  _ <- parseInsensitiveWord "WHERE" <|> unrecognisedCommandError
  _ <- many parseSpace
  cond <- parseConditionList
  _ <- parseEnding
  return cond

parseConditionList :: Parser Conditions
parseConditionList = do
  con <- parseCondition
  cons <- many parseConditionRest
  return $ Conditions $ con:cons

parseConditionRest :: Parser (String, String, String)
parseConditionRest = do
  _ <- many parseSpace
  _ <- parseInsensitiveWord "OR"
  _ <- many parseSpace
  parseCondition

parseCondition :: Parser (String, String, String)
parseCondition = do
  con1 <- parseName
  _ <- many parseSpace
  op <- parseOperator
  _ <- many parseSpace
  con2 <- parseName
  return (con1, op, con2)

parseWhereEnding :: Parser Conditions
parseWhereEnding = (\_ -> NoConditions) <$> parseEnding

parseOperator :: Parser String
parseOperator = many $ parseCaseChar isOperatorChar
--Where statement check END

--General parsers 
parseCaseChar :: (Char -> Bool) -> Parser Char
parseCaseChar f = Parser $ \inp ->
  case inp of
    "" -> Left _EMPTY_INPUT_ERROR
    (x:xs) -> if f x
      then Right (xs,x)
      else Left _ERROR_CHAR_DID_NOT_MEET_FUNC

parseChar :: Char -> Parser Char
parseChar a = Parser $ \inp ->
  case inp of
    "" -> Left _EMPTY_INPUT_ERROR
    (x:xs) -> if x == a
      then Right (xs,x)
      else Left $  _ERROR_WAS_FOUND [a] [x]

parseWordWithCustomEquality :: (String -> String -> Bool) -> String -> Parser String
parseWordWithCustomEquality f s = Parser $ \inp ->
  case inp of
    "" -> Left _EMPTY_INPUT_ERROR
    _ -> if f s $ take (length s) inp
      then Right (drop (length s) inp, take (length s) inp)
      else Left $ _ERROR_WAS_FOUND s $ take (length s) inp

parseName :: Parser String
parseName = Parser $ \inp ->
  case takeWhile validChar inp of
   "" -> Left _EMPTY_INPUT_ERROR
   xs -> Right (drop (length xs) inp, xs)

parseEnding :: Parser Char
parseEnding = do
  text <- parseOnlyUntil isSpace ';' <|> unrecognisedSymbolsAfterStatementError
  _ <- parseChar ';' <|> semicolonError
  _ <- many parseSpace
  return ';'

parseOnlyUntil :: (Char -> Bool) -> Char -> Parser Char
parseOnlyUntil f c = Parser $ \inp -> 
  case inp of
    [] -> Left _EMPTY_INPUT_ERROR
    (x:xs) -> if x == c 
      then Right (c:xs, c) 
      else if f x 
        then runParser (parseOnlyUntil f c) xs
        else Left _GENERAL_ERROR

parseSpace :: Parser Char
parseSpace = parseCaseChar isSpace

parseWord :: String -> Parser String
parseWord = parseWordWithCustomEquality (==)

parseInsensitiveWord :: String -> Parser String
parseInsensitiveWord = parseWordWithCustomEquality caseInsensitiveEquality
--General parsers END

-----------------------------------------------------------------PARSER SETUP END

------------------------------------------------------------------------EXECUTOR SETUP

--DataFrame extractor by table name
extractDataFrame :: Database -> Executor DataFrame
extractDataFrame db = Executor $ \st ->
  if elem (table st) $ map fst db
    then maybeToEither $ findTableByName db $ table st
    else Left $ _ERROR_NO_TABLE_IN_DATABASE $ table st
--DataFrame extractor by table name END

--ParseTables executor setup
executeShow :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeShow st db = 
  case st of
    ParsedTables AllT -> Right $ DataFrame [Column "table_names" StringType] $ map (\(x,xs) -> [StringValue x]) db
    ParsedTables (Table name) -> if elem name $ map (\(x,xs) -> x) db 
      then case maybeToEither $ findTableByName db name of
        Right r -> Right r
        Left l -> Left $ _ERROR_NO_TABLE_IN_DATABASE name
      else Left $ _ERROR_NO_TABLE_IN_DATABASE name
    ParsedTables NullT -> Left _ERROR_STATEMENT_NOT_INIT
--ParseTables executor setup END

--ParseTable executor setup
executeSelect :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeSelect st db = runExecutor (executorSelect db) st

executorSelect :: Database -> Executor DataFrame
executorSelect db = do
  df <- extractDataFrame db
  df1 <- executeConditions df
  executorFrom df1
--ParseTable executor setup END

--ParseTable Columns section executor 
executorFrom :: DataFrame -> Executor DataFrame
executorFrom df = Executor $ \st ->
  case st of
    ParsedTable _ AllC _ -> Right df
    ParsedTable _ (OneColumn name) _ -> getColumnWhole name df
    ParsedTable _ (ColumnList list) _ -> case columnsInTable list df of
      Right (DataFrame cols _) -> Right $ removeColumnsByName (listSubtraction (map (\(Column name _)->name) cols) list) df
      Left l -> Left l
    ParsedTable _ NullC _ -> Left _ERROR_PARSED_TABLE_NOT_INITIATED
    ParsedTable _ fun _ -> executeAggregate fun df 
--ParseTable Columns section executor END

--Aggregate function executor setup
executeAggregate :: Columns -> DataFrame -> Either ErrorMessage DataFrame
executeAggregate c df = 
  case name c of
    "max" -> executeMax (column c) df 
    "sum" -> executeSum (column c) df 
    f -> Left $ _ERROR_UNRECOGNISED_AGGREGATE f

executeMax :: Columns -> DataFrame -> Either ErrorMessage DataFrame
executeMax (OneColumn name) df = if isColumn name df
  then case getColumnWhole name df of 
    Right (DataFrame [Column name t] []) -> Right (DataFrame [Column ("max_"++name) t] [])
    Right r -> Right $ maxInColumn name r
    Left l -> Left l
  else Left $ _ERROR_COLUMN_NOT_FOUND name

executeSum :: Columns -> DataFrame -> Either ErrorMessage DataFrame
executeSum (OneColumn name) df = if isColumn name df
  then case getColumnWhole name df of 
    Right (DataFrame [Column name t] []) -> Right (DataFrame [Column ("sum_"++name) t] [])
    Right r -> case r of
      (DataFrame [Column _ t] _) -> case t of
        IntegerType -> Right $ DataFrame [Column ("sum_"++name) IntegerType] [sumUpRows (getColumnRows name df)]
        _ -> Left $ _ERROR_NOT_INTEGER_TYPE name
    Left l -> Left l
  else Left $ _ERROR_COLUMN_NOT_FOUND name 
--Aggregate function executor setup END

executeConditions :: DataFrame -> Executor DataFrame
executeConditions df = Executor $ \st ->
  case rowConditions st of
    NoConditions -> Right df
    Conditions x -> case validateConditions x df [] of
      Left l -> Left l
      Right r -> executeConditionList r df  
      
executeConditionList :: [LogOp] -> DataFrame -> Either ErrorMessage DataFrame
executeConditionList lo (DataFrame cols rows) = executeAllRows ((toInteger $ length rows)-1) lo (DataFrame cols rows)

executeAllRows :: Integer -> [LogOp] -> DataFrame -> Either ErrorMessage DataFrame
executeAllRows (-1) lo df = Right df
executeAllRows i lo df = 
  case executeRow lo df i of
    Left l -> Left l
    Right True -> executeAllRows (i-1) lo df
    Right False -> executeAllRows (i-1) lo (removeRow i df)

executeRow :: [LogOp] -> DataFrame -> Integer -> Either ErrorMessage Bool
executeRow [] _ _ = Right False
executeRow (x:xs) df id =
  case x of
    LogOp (ColumnName n1) op (ColumnName n2) -> 
      case boolSQL (getRowValue (getColumnId n1 df) (getRow id df)) (getRowValue (getColumnId n2 df) (getRow id df)) op of
        Left l -> Left l
        Right True -> Right True
        Right False -> executeRow xs df id
    LogOp (ColumnName n) op (ConData (IntegerValue i)) -> 
      case boolSQL (getRowValue (getColumnId n df) (getRow id df)) (IntegerValue i) op of
        Left l -> Left l
        Right True -> Right True
        Right False -> executeRow xs df id
    LogOp (ConData (IntegerValue i)) op (ColumnName n) -> executeRow ((LogOp (ColumnName n) op (ConData (IntegerValue i))):xs) df id
    LogOp (ConData (IntegerValue i1)) op (ConData (IntegerValue i2)) ->
      case boolSQL (IntegerValue i1) (IntegerValue i2) op of
        Left l -> Left l
        Right True -> Right True
        Right False -> executeRow xs df id

validateConditions :: [(String, String, String)] -> DataFrame -> [LogOp] -> Either ErrorMessage [LogOp]
validateConditions [] df lo = Right lo 
validateConditions ((i1,op,i2):xs) df lo = 
  case colOrVal i1 df of 
    Left l1 -> Left l1
    Right v1 -> case colOrVal i2 df of 
      Left l2 -> Left l2
      Right v2 -> case columnCheckup v1 df of
        Left l3 -> Left l3
        Right _ -> case columnCheckup v2 df of 
          Left l4 -> Left l4 
          Right _ -> validateConditions xs df (lo++[LogOp v1 op v2])

columnCheckup :: ConData -> DataFrame -> Either ErrorMessage Bool
columnCheckup (ConData (IntegerValue _)) _ = Right True
columnCheckup (ColumnName name) df = 
  case getColumnData name df of
    Left l1 -> Left l1
    Right (Column _ t) -> if t == IntegerType 
      then Right True
      else Left $ _ERROR_NOT_INTEGER_TYPE name

logicalOperatorSetup :: (String, String, String) -> DataFrame -> Either ErrorMessage LogOp
logicalOperatorSetup (in1, op, in2) df = do
  i1 <- colOrVal in1 df
  i2 <- colOrVal in2 df
  return $ LogOp i1 op i2 

colOrVal :: String -> DataFrame -> Either ErrorMessage ConData
colOrVal s df = if isColumn s df 
  then Right $ ColumnName s
  else case stringToInt s of
    Right r -> Right $ ConData $ IntegerValue $ toInteger r
    Left l -> Left $ _ERROR_BAD_CONDITION_ARGUMENT s

boolSQL :: Value -> Value -> String -> Either ErrorMessage Bool
boolSQL NullValue NullValue op = 
  case op of 
  "=" -> Right True
  "<" -> Right False
  ">" -> Right False
  ">=" -> Right True
  "<=" -> Right True
  _ -> Left $ _ERROR_UNRECOGNISED_OPERATOR op
boolSQL NullValue (IntegerValue _) _ = Right False
boolSQL (IntegerValue _) NullValue _ = Right False
boolSQL (IntegerValue a) (IntegerValue b) op =
  case op of
    "=" -> Right $ a == b
    "<" -> Right $ a < b
    ">" -> Right $ a > b
    ">=" -> Right $ a >= b
    "<=" -> Right $ a <= b
    _ -> Left $ _ERROR_UNRECOGNISED_OPERATOR op
boolSQL _ _ op = Left _ERROR_VALUE_NOT_INTEGER
------------------------------------------------------------------------EXECUTOR SETUP END