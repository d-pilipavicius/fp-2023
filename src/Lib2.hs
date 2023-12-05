{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
module Lib2(
  parseStatement,
  executeStatement,
  isUpdatingDb,
  Database
) where

import Parser
import Errors
import Executor
import DataFrame
import CustomDataTypes
import ParserConstants
import Data.Char(isSpace)
import Functions.DFOperating
import Lib1 (isRowAndColumnTypeMatching, getType, matchValueToType)
import Functions.ValueComparing
import GeneralConstants
import Control.Applicative ((<|>), Alternative (many))
import Functions.ListOperating

type Database = [(String, DataFrame)]

parseStatement :: String -> Either ErrorMessage ParsedStatement
parseStatement query =
    case runParser parseWholeStatement query of
        Left e -> Left e
        Right ("", s) -> Right s
        Right _ -> Left _ERROR_CHARACTERS_AFTER_SEMICOLON

isUpdatingDb :: String -> Bool
isUpdatingDb s = 
  case parseStatement s of
    Left _ -> False 
    Right r -> 
      case r of
        UpdateTable {} -> True
        InsertInto {} -> True
        DeleteTableElements {} -> True
        _ -> False

executeStatement :: Database -> ParsedStatement -> Either ErrorMessage DataFrame
executeStatement db st =
  case st of
    ParsedTable {} -> executeSelect st db
    ParsedTables {} -> executeShow st db
    ManyTables {} -> executeSelect st db
    UpdateTable {} -> executeUpdate st db
    InsertInto {} -> executeInsert st db
    DeleteTableElements {} -> executeDelete st db
-----------------------------------------------------------------PARSER SETUP
--Complete parse
parseWholeStatement :: Parser ParsedStatement
parseWholeStatement = do
  typeS <- parseStatementType
  case typeS of
    ParsedTables {} -> parseRestShow
    ParsedTable {} -> parseRestSelect
    UpdateTable {} -> parseRestUpdate
    DeleteTableElements {} -> parseRestDelete
    InsertInto {} -> parseRestInsert
    _ -> generalError
--Complete parse END

--Initial word
parseStatementType :: Parser ParsedStatement
parseStatementType =
  parseShow <|>
  parseSelect <|>
  parseDelete <|>
  parseUpdate <|>
  parseInsert <|> do
    phrase <- parseNotSpaceString
    unrecognisedCommandError2 phrase
--Initial word END

--Constant names parsers
parseSelect :: Parser ParsedStatement
parseSelect = (\_ -> ParsedTable "" NullC NoConditions) <$> parseInsensitiveWord "SELECT"

parseShow :: Parser ParsedStatement
parseShow = (\_ -> ParsedTables NullT) <$> parseInsensitiveWord "SHOW"

parseDelete :: Parser ParsedStatement
parseDelete = (\_ -> DeleteTableElements "" NoConditions) <$> parseInsensitiveWord "DELETE"

parseUpdate :: Parser ParsedStatement
parseUpdate = (\_ -> UpdateTable "" (ValueChanges []) NoConditions) <$> parseInsensitiveWord "UPDATE"

parseInsert :: Parser ParsedStatement
parseInsert = (\_ -> InsertInto "" NullC []) <$> parseInsensitiveWord "INSERT"
--Constant names parsers END

--Update parsing
parseRestUpdate :: Parser ParsedStatement
parseRestUpdate = do
  _ <- many parseSpace
  tName <- parseName <|> selectUnspecifiedTableError
  _ <- many parseSpace
  _ <- parseInsensitiveWord "SET" <|> syntaxErrorLikelySource "UPDATE table_name" "SET"
  _ <- many parseSpace
  updVals <- parseUpdateValuesList <|> updateUnspecifiedRowsError
  _ <- many parseSpace
  UpdateTable tName updVals <$> parseConditions

parseUpdateValuesList :: Parser Asigning
parseUpdateValuesList = do
  val <- parseUpdateValue
  vals <- many parseUpdateValueRest
  return $ ValueChanges (val:vals)   

parseUpdateValue :: Parser Asigning
parseUpdateValue = do
  col <- parseName
  _ <- many parseSpace
  _ <- parseChar '='
  _ <- many parseSpace
  input <- parseInsertValue
  return $ Asign {asgCol = OneColumn col, asgValue = input}

parseUpdateValueRest :: Parser Asigning
parseUpdateValueRest = do
  _ <- many parseSpace
  _ <- parseChar ','
  _ <- many parseSpace
  parseUpdateValue
--Update parsing END

--Delete parsing
parseRestDelete :: Parser ParsedStatement
parseRestDelete = do
  _ <- many parseSpace
  _ <- parseInsensitiveWord "FROM" <|> syntaxErrorLikelySource "DELETE" "FROM"
  _ <- many parseSpace
  tName <- parseName <|> deleteUnspecifiedTableError
  _ <- many parseSpace
  DeleteTableElements tName <$> parseConditions
--Delete parsing END

--Insert parsing
parseRestInsert :: Parser ParsedStatement
parseRestInsert = do
  _ <- many parseSpace
  _ <- parseInsensitiveWord "INTO" <|> syntaxErrorLikelySource "INSERT" "INTO"
  _ <- many parseSpace
  tName <- parseName <|> insertUnspecifiedTableError
  _ <- many parseSpace
  cols <- parseInsertColumns <|> parseInsertNoColList
  _ <- many parseSpace
  _ <- parseInsensitiveWord "VALUES" <|> syntaxErrorLikelySource "INSERT INTO table_name" "VALUES"
  _ <- many parseSpace
  vals <- parseInsertValuesWholeList 
  _ <- parseEnding
  return $ InsertInto tName cols vals

parseInsertNoColList :: Parser Columns
parseInsertNoColList = Parser $ \inp ->
  case runParser (parseChar '(') inp of
    Left _ -> Right (inp, AllC)
    Right _ -> Left _ERROR_INSERT_INTO_SYNTAX

parseInsertColumns :: Parser Columns
parseInsertColumns = do
  _ <- parseChar '('
  _ <- many parseSpace
  cols <- parseColumnList
  _ <- many parseSpace
  _ <- parseChar ')'
  return cols

parseInsertValuesWholeList :: Parser [[String]]
parseInsertValuesWholeList = do
  vals <- parseInsertValuesWhole
  valsList <- many parseInsertValuesWholeAfter
  return $ vals:valsList

parseInsertValuesWholeAfter :: Parser [String]
parseInsertValuesWholeAfter = do
  _ <- many parseSpace
  _ <- parseChar ','
  _ <- many parseSpace
  parseInsertValuesWhole

parseInsertValuesWhole :: Parser [String]
parseInsertValuesWhole = do
  _ <- parseChar '(' <|> insertMissingValuesError
  _ <- many parseSpace
  vals <- parseInsertValuesList
  _ <- many parseSpace
  _ <- parseChar ')'
  return vals

parseInsertValuesList :: Parser [String]
parseInsertValuesList = do
  val <- parseInsertValue
  vals <- many parseInsertValues
  return (val:vals)

parseInsertValue :: Parser String
parseInsertValue = Parser $ \inp ->
  case inp of
    "" -> Left _EMPTY_INPUT_ERROR
    (x:xs) -> 
      if x == '\''
        then case '\'':takeWhile (/= '\'') xs of
          out -> do
            _ <- trueOrError (length out /= length inp) _ERROR_CLOSING_APOSTROPHE_MISSING
            Right (drop (length out +1) inp, out++"\'")
        else runParser parseInsertNameOrFun inp

parseInsertNameOrFun :: Parser String
parseInsertNameOrFun = Parser $ \inp ->
  case runParser parseFunction inp of
    Right (out, f) -> do
      let fName = name f
      let col = column f
      let cName = insertForFuncResult col
      Right (out, fName ++ "(" ++ cName ++ ")")
    Left _ -> 
      case runParser parseName inp of
      Right r -> Right r
      Left l -> Left l

insertForFuncResult :: Columns -> String
insertForFuncResult c =
  case c of
    AllC -> "*"
    NullC -> ""
    OneColumn name -> name

parseInsertValues :: Parser String
parseInsertValues = do
  _ <- many parseSpace
  _ <- parseChar ','
  _ <- many parseSpace
  parseInsertValue
--Insert parsing END

--Table names
parseRestShow :: Parser ParsedStatement
parseRestShow = do
  _ <- many parseSpace
  stat <- parseShowTableNames
  _ <- parseEnding
  return stat

parseShowTableNames :: Parser ParsedStatement
parseShowTableNames = fmap ParsedTables (parseAllT <|> parseTable) <|> showNoTablesError

parseAllT :: Parser Tables
parseAllT = (\_ -> AllT) <$> parseInsensitiveWord "TABLES"

parseTable :: Parser Tables
parseTable = do
  _ <- parseInsensitiveWord "TABLE" <|> showNoTableError
  _ <- many parseSpace
  fmap Table parseName
--Table names END

--Select statement setup
parseRestSelect :: Parser ParsedStatement
parseRestSelect = do
  _ <- many parseSpace
  pColumns <- parseColumnList <|> selectNoColumnsError
  _ <- many parseSpace
  _ <- parseInsensitiveWord "FROM" <|> syntaxErrorLikelySource "SELECT" "FROM"
  _ <- many parseSpace
  parseFromTable pColumns <|> parseFromTables pColumns 

parseFromTable :: Columns -> Parser ParsedStatement
parseFromTable cols = do
  tableName <- parseName 
  _ <- many parseSpace
  ParsedTable tableName cols <$> parseConditions

parseFromTables :: Columns -> Parser ParsedStatement
parseFromTables cols = do
  tableName <- parseName <|> selectUnspecifiedTablesError
  tableNames <- many parseNames
  _ <- many parseSpace
  ManyTables (tableName:tableNames) cols <$> parseConditions
--Select statement setup END

--Column names
parseAllC :: Parser Columns
parseAllC = (\_ -> AllC) <$> parseChar '*'

parseSingleC :: Parser Columns
parseSingleC = do fmap OneColumn parseName

parseFunction :: Parser Columns
parseFunction = do
  fun <- parseName
  _ <- many parseSpace
  _ <- parseChar '('
  _ <- many parseSpace
  pColumn <- 
    parseAllC 
    <|> parseSingleC 
    <|> do
      _ <- parseOnlyUntil isSpace ')'
      return NullC 
    <|> unrecognisedFunInput
  _ <- many parseSpace
  _ <- parseChar ')'
  return $ Func {name = stringToLower fun, column = pColumn}

parseColumnList :: Parser Columns
parseColumnList = do
  col <- parseFunction <|> parseSingleC <|> parseAllC
  other <- many parseColumnsRest
  return $ ColumnList $ col:other

parseColumnsRest :: Parser Columns
parseColumnsRest = do
    _ <- many parseSpace
    _ <- parseChar ','
    _ <- many parseSpace
    parseFunction <|> parseSingleC <|> parseAllC
--Column names END

--Where statement check
parseConditions :: Parser Conditions
parseConditions = parseWhereEnding <|> parseWhere

parseWhere :: Parser Conditions
parseWhere = do
  _ <- parseInsensitiveWord "WHERE" <|> unrecognisedCommandError
  _ <- many parseSpace
  cond <- parseConditionList <|> syntaxErrorLikelySource "WHERE" "the conditions"
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
parseWordWithCustomEquality f s = do
  word <- parseName <|> nothingToMatchAgainst
  if f s word
    then return word
    else wordNotMatching s word

parseName :: Parser String
parseName = Parser $ \inp ->
  case takeWhile validChar inp of
   "" -> Left _EMPTY_INPUT_ERROR
   xs -> Right (drop (length xs) inp, xs)

parseNames :: Parser String
parseNames = do
  _ <- many parseSpace
  _ <- parseChar ','
  _ <- many parseSpace
  parseName

parseEnding :: Parser Char
parseEnding = do
  _ <- parseOnlyUntil isSpace ';' <|> unrecognisedSymbolsAfterStatementError
  _ <- parseChar ';' <|> semicolonError
  _ <- many parseSpace
  return ';'

parseOnlyUntil :: (Char -> Bool) -> Char -> Parser Char
parseOnlyUntil f c = Parser $ \inp ->
  case inp of
    [] -> Left $ _EMPTY_INPUT_ERROR
    (x:xs) -> if x == c
      then Right (c:xs, c)
      else if f x
        then runParser (parseOnlyUntil f c) xs
        else Left _GENERAL_ERROR

parseSpace :: Parser Char
parseSpace = parseCaseChar isSpace

parseInsensitiveWord :: String -> Parser String
parseInsensitiveWord = parseWordWithCustomEquality caseInsensitiveEquality

parseNotSpaceString :: Parser String
parseNotSpaceString = Parser $ \inp ->
  case inp of
    "" -> Left _EMPTY_INPUT_ERROR
    _ -> case takeWhile (not . isSpace) inp of
      "" -> Left _ERROR_SPACES_IN_THE_WAY
      rez -> Right (drop (length rez) inp, rez)
--General parsers END

-----------------------------------------------------------------PARSER SETUP END
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------EXECUTOR SETUP
--DataFrame extractor by table name
extractDataFrame :: Database -> Executor DataFrame
extractDataFrame db = Executor $ \st ->
  case findTableByNameNoCase db $ table st of
    Just tb -> Right tb
    Nothing -> Left $ _ERROR_NO_TABLE_IN_DATABASE $ table st

manyTablesOrSingleDataFrame :: Database -> Executor DataFrame
manyTablesOrSingleDataFrame db = Executor $ \inp -> 
  case inp of
    ManyTables {} -> runExecutor (extractDataFrames db) inp
    ParsedTable {} -> runExecutor (extractDataFrame db) inp
    _ -> Left _GENERAL_ERROR

extractWhileEmptyWithPrefix :: Database -> [String] -> [DataFrame] -> Either ErrorMessage DataFrame
extractWhileEmptyWithPrefix _ [] dfs = Right $ multiplyMultipleTables dfs 
extractWhileEmptyWithPrefix db (x:xs) dfs =
  case findTableByNameNoCase db x of
    Just tb -> extractWhileEmptyWithPrefix db xs (dfs++[addTableNamePrefix tb x])
    Nothing -> Left $ _ERROR_NO_TABLE_IN_DATABASE x

extractDataFrames :: Database -> Executor DataFrame
extractDataFrames db = Executor $ \st ->
  if hasDuplicates $ tNames st 
    then Left _ERROR_TABLE_MULTIPLE_INSTANCES
    else extractWhileEmptyWithPrefix db (tNames st) []
--DataFrame extractor by table name END

--ParseTables executor setup
--NOTE: This only works with ParsedTables
executeShow :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeShow st db =
  case tables st of
    AllT -> Right $ DataFrame [Column "table_names" StringType] $ map (\(x,_) -> [StringValue x]) db
    Table tName -> do
      _ <- trueOrError (elem tName $ map fst db) $ _ERROR_NO_TABLE_IN_DATABASE tName
      case maybeToEither $ findTableByNameNoCase db tName of
        Right r -> Right r
        Left _ -> Left $ _ERROR_NO_TABLE_IN_DATABASE tName
    NullT -> Left _ERROR_STATEMENT_NOT_INIT
--ParseTables executor setup END

--Select statement execution setup
--ParseTable/ManyTables executor setup
executeSelect :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeSelect st db = runExecutor (executorSelect db) st

executorSelect :: Database -> Executor DataFrame
executorSelect db = do
  df <- manyTablesOrSingleDataFrame db
  df1 <- executeConditions df
  executorFrom df1
--ParseTable/ManyTables executor setup END

--Columns executor 
--NOTE: THIS ONLY WORKS WITH ParsedTable
executorFrom :: DataFrame -> Executor DataFrame
executorFrom df = Executor $ \st ->
  case columns st of
    AllC -> Right df
    OneColumn cName -> getColumnWhole cName df
    ColumnList list -> executeColumns list df
    NullC -> Left _ERROR_PARSED_TABLE_NOT_INITIATED
    fun -> executeFunctions fun df

executeColumns :: [Columns] -> DataFrame -> Either ErrorMessage DataFrame
executeColumns cols df = do
  legal <- isLegalColumns cols
  _ <- trueOrError legal _ERROR_AGGREGATE_FUNCTIONS
  executeColumns1 cols df $ DataFrame [] []

executeColumns1 :: [Columns] -> DataFrame -> DataFrame -> Either ErrorMessage DataFrame
executeColumns1 [] _ dfAns = Right dfAns
executeColumns1 (x:xs) df dfAns = do
  case x of
    NullC -> executeColumns1 xs df dfAns
    AllC -> do
      combinedDf <- connectTwoTables dfAns df
      executeColumns1 xs df combinedDf
    OneColumn val -> do
      col <- getColumnWhole val df
      combinedDf <- connectTwoTables dfAns col
      executeColumns1 xs df combinedDf
    Func {} -> do
      col <- executeFunctions x df
      if dfAns == DataFrame [] []
        then executeColumns1 xs df col
        else executeColumns1 xs df $ multiplyTwoTables dfAns col
    _ -> Left _GENERAL_ERROR

isLegalColumns :: [Columns] -> Either ErrorMessage Bool
isLegalColumns [] = Right True
isLegalColumns (x:xs) = 
  case x of
    Func _ _ -> funFound x xs
    OneColumn _ -> colFound xs
    AllC -> colFound xs
    _ -> Left _GENERAL_ERROR

funFound :: Columns -> [Columns] -> Either ErrorMessage Bool
funFound _ [] = Right True
funFound c (x:xs) = 
  case c of
    Func "max" _ ->
      case x of
        Func {} -> funFound c xs
        _ -> Right False
    Func "sum" _ ->
      case x of
        Func {} -> funFound c xs
        _ -> Right False
    Func {} -> isLegalColumns (x:xs)
    _ -> Left _GENERAL_ERROR

colFound :: [Columns] -> Either ErrorMessage Bool 
colFound [] = Right True 
colFound (x:xs) = 
  case x of
    Func "max" _ -> Right False
    Func "sum" _ -> Right False
    Func {} -> colFound xs
    AllC -> colFound xs
    OneColumn _ -> colFound xs
    _ -> Left _GENERAL_ERROR
--Columns executor END

--Function executor setup
executeFunctions :: Columns -> DataFrame -> Either ErrorMessage DataFrame
executeFunctions c df =
  case name c of
    "max" -> executeMax (column c) df
    "sum" -> executeSum (column c) df
    "now" -> do executeNow (column c)
    f -> Left $ _ERROR_UNRECOGNISED_AGGREGATE f

executeMax :: Columns -> DataFrame -> Either ErrorMessage DataFrame
executeMax (OneColumn cName) df = do
  _ <- trueOrError (isColumn df cName) $ _ERROR_COLUMN_NOT_FOUND cName
  case getColumnWhole cName df of
    Right (DataFrame [Column _ t] []) -> Right (DataFrame [Column ("max_"++cName) t] [])
    Right r -> Right $ maxInColumn cName r
    Left l -> Left l
executeMax NullC _ = Left $ _ERROR_FUNCTION_NO_ARGUMENT "MAX()"
executeMax _ _ = Left _GENERAL_ERROR

executeSum :: Columns -> DataFrame -> Either ErrorMessage DataFrame
executeSum (OneColumn cName) df = do
  _ <- trueOrError (isColumn df cName) $ _ERROR_COLUMN_NOT_FOUND cName
  case getColumnWhole cName df of
    Right (DataFrame [Column _ t] []) -> Right (DataFrame [Column ("sum_"++cName) t] [])
    Right r -> 
      case r of
        (DataFrame [Column _ t] _) -> 
          case t of
            IntegerType -> Right $ DataFrame [Column ("sum_"++cName) IntegerType] [sumUpRows (getColumnRows cName df)]
            _ -> Left $ _ERROR_NOT_INTEGER_TYPE cName
        _ -> Left _GENERAL_ERROR
    Left l -> Left l
executeSum NullC _ = Left $ _ERROR_FUNCTION_NO_ARGUMENT "SUM()"
executeSum _ _ = Left _GENERAL_ERROR

executeNow :: Columns -> Either ErrorMessage DataFrame
executeNow NullC = Right $ DataFrame [Column _TIME_COLUMN_NAME StringType] [[StringValue _TIME_INSERT_VALUE]]
executeNow (OneColumn _) = Left _ERROR_FUNCTION_NOW_FOUND_PARAMETERS
executeNow _ = Left _GENERAL_ERROR
--Function executor setup END

--Conditions execution
executeConditions :: DataFrame -> Executor DataFrame
executeConditions df = Executor $ \st ->
  case rowConditions st of
    NoConditions -> Right df
    Conditions x -> 
      case validateConditions x df [] of
        Left l -> Left l
        Right r -> executeConditionList r df

executeConditionList :: [LogOp] -> DataFrame -> Either ErrorMessage DataFrame
executeConditionList lo (DataFrame cols rows) = executeAllRows ((toInteger $ length rows)-1) lo (DataFrame cols rows)

executeAllRows :: Integer -> [LogOp] -> DataFrame -> Either ErrorMessage DataFrame
executeAllRows (-1) _ df = Right df
executeAllRows i lo df = do 
  boolVar <- executeRow lo df i
  if boolVar
    then executeAllRows (i-1) lo df
    else executeAllRows (i-1) lo (removeRow i df)

executeRow :: [LogOp] -> DataFrame -> Integer -> Either ErrorMessage Bool
executeRow [] _ _ = Right False
executeRow (x:xs) df id' = 
  case x of
    LogOp (ColumnName n1) op (ColumnName n2) -> do
      boolVal <- boolSQL (getRowValue (getColumnId n1 df) (getRow id' df)) (getRowValue (getColumnId n2 df) (getRow id' df)) op
      if boolVal 
        then Right True
        else executeRow xs df id'
    LogOp (ColumnName n) op (ConData (IntegerValue i)) -> do
      boolVal <- boolSQL (getRowValue (getColumnId n df) (getRow id' df)) (IntegerValue i) op
      if boolVal
        then Right True
        else executeRow xs df id'
    LogOp (ConData (IntegerValue i1)) op (ConData (IntegerValue i2)) -> do
      boolVal <- boolSQL (IntegerValue i1) (IntegerValue i2) op
      if boolVal
        then Right True
        else executeRow xs df id'
    _ -> Left _GENERAL_ERROR

validateConditions :: [(String, String, String)] -> DataFrame -> [LogOp] -> Either ErrorMessage [LogOp]
validateConditions [] _ lo = Right lo
validateConditions ((i1,op,i2):xs) df lo = do 
  v1 <- colOrVal i1 df
  v2 <- colOrVal i2 df
  boolVal1 <- columnCheckup v1 df
  boolVal2 <- columnCheckup v2 df
  if boolVal1 || boolVal2
    then if boolVal1
      then validateConditions xs df (lo++[LogOp v1 op v2])
      else validateConditions xs df (lo++[LogOp v2 op v1])
    else validateConditions xs df (lo++[LogOp v1 op v2])

columnCheckup :: ConData -> DataFrame -> Either ErrorMessage Bool
columnCheckup (ConData (IntegerValue _)) _ = Right False
columnCheckup (ColumnName cName) df = do
  cols <- getColumnData cName df
  trueOrError (getType cols == IntegerType) $ _ERROR_NOT_INTEGER_TYPE cName
  return True
columnCheckup _ _ = Left _GENERAL_ERROR

colOrVal :: String -> DataFrame -> Either ErrorMessage ConData
colOrVal s df =
  if isColumn df s
    then Right $ ColumnName s
    else 
      case stringToInt s of
        Right r -> Right $ ConData $ IntegerValue r
        Left _ -> Left $ _ERROR_BAD_CONDITION_ARGUMENT s

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
boolSQL _ _ _ = Left _ERROR_VALUE_NOT_INTEGER
--Conditions execution END
--Select statement execution setup END

--Update execution
executeUpdate :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeUpdate st db = runExecutor (executorUpdate db) st 

executorUpdate :: Database -> Executor DataFrame
executorUpdate db = do 
  df <- extractDataFrame db
  cons <- executorUpdateGetLogOps df
  values <- executorUpdateValidateAssignments df
  Executor $ \_ -> updateCheckAndChange df cons values 0
  
executorUpdateGetLogOps :: DataFrame -> Executor [LogOp]
executorUpdateGetLogOps df = Executor $ \inp -> do
  let cons = rowConditions inp
  case cons of
    NoConditions -> return [LogOp (ConData $ IntegerValue 0) "=" (ConData $ IntegerValue 0)]
    Conditions c -> validateConditions c df [] 

executorUpdateValidateAssignments :: DataFrame -> Executor (Dictionary String Value)
executorUpdateValidateAssignments df = Executor $ \inp -> do
  let assign = tableChanges inp
  case assign of
    ValueChanges vals -> updateValidateAssignments df vals
    _ -> Left _GENERAL_ERROR

updateValidateAssignments :: DataFrame -> [Asigning] -> Either ErrorMessage (Dictionary String Value)
updateValidateAssignments df as = updateValidateAssignments1 df as []

updateValidateAssignments1 :: DataFrame -> [Asigning] -> Dictionary String Value -> Either ErrorMessage (Dictionary String Value) 
updateValidateAssignments1 _ [] dict = Right dict
updateValidateAssignments1 df (x:xs) dict = do
  valid <- updateValidateAssingment df x
  case combineDict dict valid of
    Right r -> updateValidateAssignments1 df xs r
    Left _ -> Left _ERROR_EXECUTE_DUPLICATE_NAMES

updateCheckAndChange :: DataFrame -> [LogOp] -> Dictionary String Value -> Integer -> Either ErrorMessage DataFrame
updateCheckAndChange df lo d i = 
  if i <= dfRowCount df 
    then do
      boolVal <- executeRow lo df i
      if boolVal
        then do
          let onlyRow = DataFrame (dfCol df) [getRow i df]
          let updatedRow = updateRow onlyRow d 0
          let cols = dfCol df
          let updatedRows = changeElementN i updatedRow (dfRows df) 
          updateCheckAndChange (DataFrame cols updatedRows) lo d (i+1)
        else updateCheckAndChange df lo d (i+1)
    else return df

updateRow :: DataFrame -> Dictionary String Value -> Integer -> Row
updateRow df d i =
  if i == dfColCount df
    then getRow 0 df
    else do
      case dictGetByKey d $ colName (dfCol df !! fromInteger i) of
        Just a -> do
          let uRow = changeElementN i a (getRow 0 df)
          let cols = dfCol df
          let newDf = DataFrame cols [uRow]
          updateRow newDf d (i+1)
        Nothing -> updateRow df d (i+1)

updateValidateAssingment :: DataFrame -> Asigning -> Either ErrorMessage (Dictionary String Value) 
updateValidateAssingment df val =
  case asgCol val of
    OneColumn c -> do
      _ <- trueOrError (isColumn df c) $ _ERROR_COLUMN_NOT_FOUND c
      dfVal <- predictValue (asgValue val) df
      col <- getColumnData c df
      let boolVal = dfVal == NullValue || getType col == (\(Just a) -> a) (matchValueToType dfVal)
      _ <- trueOrError boolVal _ERROR_EXECUTE_VALUE_DOES_NOT_ALIGN_WITH_TYPE
      return [(c, dfVal)]
    _ -> Left _GENERAL_ERROR

--Update execution END

--Insert execution
executeInsert :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeInsert st db = runExecutor (executorInsert db) st 

executorInsert :: Database -> Executor DataFrame
executorInsert db = do 
  df <- extractDataFrame db
  executorInsertAllRows df

executorInsertAllRows :: DataFrame -> Executor DataFrame
executorInsertAllRows df = Executor $ \inp -> do
  let cols = columns inp
  if cols == AllC
    then 
      insertAllRows cols (insertedValues inp) df
    else do
      let colList = (\(ColumnList l) -> l) cols
      _ <- trueOrError (not $ hasDuplicates colList) _ERROR_EXECUTE_DUPLICATE_NAMES
      insertAllRows cols (insertedValues inp) df

insertAllRows :: Columns -> [[String]] -> DataFrame -> Either ErrorMessage DataFrame
insertAllRows _ [] df = Right df
insertAllRows cols (x:xs) df = do
  newDf <- insertOneRow cols x df
  insertAllRows cols xs newDf

insertOneRow :: Columns -> [String] -> DataFrame -> Either ErrorMessage DataFrame
insertOneRow cols vals df = do
  _ <- trueOrError (toInteger (length vals) <= dfColCount df) "Cannot insert more values than column count to a table."
  case cols of
    AllC -> do
      row <- transformToRow df vals (dfColCount df) []
      let dfrows = dfRows df
      let dfcols = dfCol df
      _ <- trueOrError (isRowAndColumnTypeMatching row dfcols) _ERROR_EXECUTE_VALUE_DOES_NOT_ALIGN_WITH_TYPE
      return $ DataFrame (dfCol df) $ dfrows++[row]
    ColumnList [] -> Right df
    ColumnList x -> do
      colNames <- insertOnlyColumnNames x
      _ <- areColumns df colNames
      _ <- trueOrError (length colNames == length vals) _ERROR_EXECUTE_INSERT_NOT_ENOUGH_VALUES
      let mapped = mapLists colNames vals
      let inOrderRows = insertRowCorrectOrder mapped $ dfCol df
      row <- transformToRow df inOrderRows (dfColCount df) []
      let dfrows = dfRows df
      let dfcols = dfCol df
      _ <- trueOrError (isRowAndColumnTypeMatching row dfcols) _ERROR_EXECUTE_VALUE_DOES_NOT_ALIGN_WITH_TYPE
      return $ DataFrame dfcols $ dfrows++[row]
    _ -> Left _GENERAL_ERROR

insertRowCorrectOrder :: Dictionary String String -> [Column] -> [String]
insertRowCorrectOrder dict col = insertRowCorrectOrder1 dict col []

insertRowCorrectOrder1 :: Dictionary String String -> [Column] -> [String] -> [String]
insertRowCorrectOrder1 _ [] ans = ans
insertRowCorrectOrder1 dict (x:xs) ans = 
  case dictGetByKey dict $ colName x of
    Nothing -> insertRowCorrectOrder1 dict xs $ ans++["null"]
    Just val -> insertRowCorrectOrder1 dict xs $ ans++[val]

insertOnlyColumnNames :: [Columns] -> Either ErrorMessage [String]
insertOnlyColumnNames c = insertOnlyColumnNames1 c []

insertOnlyColumnNames1 :: [Columns] -> [String] -> Either ErrorMessage [String]
insertOnlyColumnNames1 [] s = Right s
insertOnlyColumnNames1 (x:xs) s = 
  case x of
    OneColumn cName -> insertOnlyColumnNames1 xs $ s++[cName]
    _ -> Left _ERROR_EXECUTE_NOT_COLUMN_NAME

transformToRow :: DataFrame -> [String] -> Integer -> Row -> Either ErrorMessage Row
transformToRow _ _ 0 rest = Right rest
transformToRow df [] i rest = transformToRow df [] (i-1) $ rest++[NullValue]
transformToRow df (x:xs) i rest = do
  value <- predictValue x df 
  transformToRow df xs (i-1) $ rest ++ [value]

predictValue :: String -> DataFrame -> Either ErrorMessage Value
predictValue s df = 
  case parseIntegerValue s of
    Right r -> Right r
    Left _ -> 
      case parseBoolValue s of
        Right r -> Right r
        Left _ ->
          case parseNullValue s of 
            Right r -> Right r
            Left _ -> 
              case parseStringValue s of
                Right r -> Right r
                Left _ -> 
                  case insertFunc s df of
                    Right r -> Right r
                    Left _ -> Left $ _ERROR_NOT_A_VALUE s

insertFunc :: String -> DataFrame -> Either ErrorMessage Value
insertFunc s df = 
  case runParser parseFunction s of
    Right (_,f) -> 
      case executeFunctions f df of
        Left l -> Left l
        Right r ->
          case r of
            DataFrame _ [x:_] -> Right x
            _ -> Left _GENERAL_ERROR
    Left l -> Left l 

insertIsNow :: String -> Bool
insertIsNow s = 
  case runParser parseFunction s of 
    Right (_,f) -> 
      case name f of
        "now" -> 
          case column f of
            NullC -> True
            _ -> False
        _ -> False
    Left _ -> False        
--Insert execution END

--Delete execution
executeDelete :: ParsedStatement -> Database -> Either ErrorMessage DataFrame
executeDelete st db = runExecutor (executorDelete db) st 

executorDelete :: Database -> Executor DataFrame
executorDelete db = do 
  df <- extractDataFrame db
  df1 <- executeConditions df
  let rows1 = dfRows df1
  return $ removeRowsFromDF df rows1
--Delete execution END
------------------------------------------------------------------------EXECUTOR SETUP END
