module SQLParserImplementation (
  parseStatement,
  parseFunction
) where
import SQLParser
import Errors
import SQLCustomDataTypes
import SQLParserConstants
import Data.Char(isSpace)
import Functions.ValueComparing
import Control.Monad.Trans.State.Strict (get, put, evalState)
import Control.Monad.Trans.Except (throwE, catchE, runExceptT)
import Control.Monad.Trans.Class(lift)
import DataFrame (Column(..), ColumnType(..))

-- CUSTOM OPERATORS
-- <|> OPERATOR DOES NOT WORK WITH ExceptT THE SAME WAY IT WORKED WITH OUR ORIGINAL PARSER
-- <:> OPERATOR FIXES THIS PROBLEM AND ALLOWS ExceptT PARSER TO OPERATE THE SAME WAY
(<:>) :: Parser a -> Parser a -> Parser a
p1 <:> p2 = do
  inp <- lift get
  catchE p1 (\_ -> do
    lift $ put inp
    p2)

manyForParser :: Parser a -> Parser [a]
manyForParser p = manyForParser1 p $ return []

manyForParser1 :: Parser a -> Parser [a] -> Parser [a] 
manyForParser1 p vals = do
  valSev <- vals
  inp <- lift get
  case evalState (runExceptT p) inp of 
    Right _ -> do 
      val <- p
      manyForParser1 p $ return (valSev++[val])
    Left _ -> do
      lift $ put inp
      vals
--CUSTOM OPERATORS END

parseStatement :: String -> Either ErrorMessage ParsedStatement
parseStatement query =
    case runParser parseWholeStatement query of
      (f,s) -> case f of
        Left e -> Left e
        r -> case s of
          "" -> r
          _ -> Left _ERROR_CHARACTERS_AFTER_SEMICOLON

-----------------------------------------------------------------PARSER SETUP
--Complete parse
parseWholeStatement :: Parser ParsedStatement
parseWholeStatement = do
  manyForParser parseSpace
  typeS <- parseStatementType
  case typeS of
    ParsedTables {} -> parseRestShow
    ParsedTable {} -> parseRestSelect
    UpdateTable {} -> parseRestUpdate
    DeleteTableElements {} -> parseRestDelete
    InsertInto {} -> parseRestInsert
    CreateTable {} -> parseRestCreate
    DropTable {} -> parseRestDrop
    _ -> generalError
--Complete parse END

--Initial word
parseStatementType :: Parser ParsedStatement
parseStatementType =
  parseShow <:>
  parseDrop <:>
  parseCreate <:>
  parseSelect <:>
  parseDelete <:>
  parseUpdate <:>
  parseInsert <:> do
    phrase <- parseNotSpaceString
    unrecognisedCommandError2 phrase
--Initial word END

--Constant names parsers
parseSelect :: Parser ParsedStatement
parseSelect = (\_ -> ParsedTable "" NullC NoConditions NullOr) <$> parseInsensitiveWord "SELECT"

parseShow :: Parser ParsedStatement
parseShow = (\_ -> ParsedTables NullT) <$> parseInsensitiveWord "SHOW"

parseDelete :: Parser ParsedStatement
parseDelete = (\_ -> DeleteTableElements "" NoConditions) <$> parseInsensitiveWord "DELETE"

parseUpdate :: Parser ParsedStatement
parseUpdate = (\_ -> UpdateTable "" (ValueChanges []) NoConditions) <$> parseInsensitiveWord "UPDATE"

parseInsert :: Parser ParsedStatement
parseInsert = (\_ -> InsertInto "" NullC []) <$> parseInsensitiveWord "INSERT"

parseCreate :: Parser ParsedStatement
parseCreate = (\_ -> CreateTable "" []) <$> parseInsensitiveWord "CREATE"

parseDrop :: Parser ParsedStatement
parseDrop = (\_ -> DropTable "") <$> parseInsensitiveWord "DROP"
--Constant names parsers END

--Create parsing
parseRestCreate :: Parser ParsedStatement
parseRestCreate = do
  manyForParser parseSpace
  parseInsensitiveWord "TABLE" -- Add an error here
  manyForParser parseSpace
  tName <- parseName
  manyForParser parseSpace
  parseChar '(' -- Add an error about missing opening bracket
  manyForParser parseSpace
  cols <- parseCreateColumns -- Add an error about bad column naming or parsing or ect
  manyForParser parseSpace
  parseChar ')' -- Add an error about missing closing bracket
  parseEnding
  return $ CreateTable tName cols
  
parseCreateColumns :: Parser [Column]
parseCreateColumns = do
  col <- parseCreateColumn
  cols <- manyForParser parseCreateColumnRest
  return (col:cols)

parseCreateColumn :: Parser Column
parseCreateColumn = do
  cName <- parseName
  manyForParser parseSpace
  cType <- parseName
  colType <- return $ do
    case stringToLower cType of
      "integertype" -> Just IntegerType
      "stringtype" -> Just StringType
      "booltype" -> Just BoolType
      _ -> Nothing
  case colType of
    Nothing -> throwE $ "Type "++cType++" not recognised, only IntegerType, StringType and BoolType can be entered."
    Just a -> return $ Column cName a

parseCreateColumnRest :: Parser Column
parseCreateColumnRest = do
  manyForParser parseSpace
  parseChar ','
  manyForParser parseSpace
  parseCreateColumn

--Create parsing END

--Drop parsing
parseRestDrop :: Parser ParsedStatement
parseRestDrop = do
  manyForParser parseSpace
  parseInsensitiveWord "TABLE" --Maybe add an error here 
  manyForParser parseSpace
  tName <- parseName --Add an error here 
  parseEnding
  return $ DropTable tName 
--Drop parsing END 

--Update parsing
parseRestUpdate :: Parser ParsedStatement
parseRestUpdate = do
  _ <- manyForParser parseSpace
  tName <- parseName <:> selectUnspecifiedTableError
  _ <- manyForParser parseSpace
  _ <- parseInsensitiveWord "SET" <:> syntaxErrorLikelySource "UPDATE table_name" "SET"
  _ <- manyForParser parseSpace
  updVals <- parseUpdateValuesList <:> updateUnspecifiedRowsError
  _ <- manyForParser parseSpace
  UpdateTable tName updVals <$> parseConditions

parseUpdateValuesList :: Parser Asigning
parseUpdateValuesList = do
  val <- parseUpdateValue
  vals <- manyForParser parseUpdateValueRest
  return $ ValueChanges (val:vals)   

parseUpdateValue :: Parser Asigning
parseUpdateValue = do
  col <- parseName
  _ <- manyForParser parseSpace
  _ <- parseChar '='
  _ <- manyForParser parseSpace
  input <- parseInsertValue
  return $ Asign {asgCol = OneColumn col, asgValue = input}

parseUpdateValueRest :: Parser Asigning
parseUpdateValueRest = do
  _ <- manyForParser parseSpace
  _ <- parseChar ','
  _ <- manyForParser parseSpace
  parseUpdateValue
--Update parsing END

--Delete parsing
parseRestDelete :: Parser ParsedStatement
parseRestDelete = do
  _ <- manyForParser parseSpace
  _ <- parseInsensitiveWord "FROM" <:> syntaxErrorLikelySource "DELETE" "FROM"
  _ <- manyForParser parseSpace
  tName <- parseName <:> deleteUnspecifiedTableError
  _ <- manyForParser parseSpace
  DeleteTableElements tName <$> parseConditions
--Delete parsing END

--Insert parsing
parseRestInsert :: Parser ParsedStatement
parseRestInsert = do
  _ <- manyForParser parseSpace
  _ <- parseInsensitiveWord "INTO" <:> syntaxErrorLikelySource "INSERT" "INTO"
  _ <- manyForParser parseSpace
  tName <- parseName <:> insertUnspecifiedTableError
  _ <- manyForParser parseSpace
  cols <- parseInsertColumns <:> parseInsertNoColList
  _ <- manyForParser parseSpace
  _ <- parseInsensitiveWord "VALUES" <:> syntaxErrorLikelySource "INSERT INTO table_name" "VALUES"
  _ <- manyForParser parseSpace
  vals <- parseInsertValuesWholeList 
  _ <- parseEnding
  return $ InsertInto tName cols vals

parseInsertNoColList :: Parser Columns
parseInsertNoColList = do 
  inp <- lift get
  case fst $ runParser (parseChar '(') inp of 
    Left _-> return AllC
    Right _ -> throwE _ERROR_INSERT_INTO_SYNTAX

parseInsertColumns :: Parser Columns
parseInsertColumns = do
  _ <- parseChar '('
  _ <- manyForParser parseSpace
  cols <- parseColumnList
  _ <- manyForParser parseSpace
  _ <- parseChar ')'
  return cols

parseInsertValuesWholeList :: Parser [[String]]
parseInsertValuesWholeList = do
  vals <- parseInsertValuesWhole
  valsList <- manyForParser parseInsertValuesWholeAfter
  return $ vals:valsList

parseInsertValuesWholeAfter :: Parser [String]
parseInsertValuesWholeAfter = do
  _ <- manyForParser parseSpace
  _ <- parseChar ','
  _ <- manyForParser parseSpace
  parseInsertValuesWhole

parseInsertValuesWhole :: Parser [String]
parseInsertValuesWhole = do
  _ <- parseChar '(' <:> insertMissingValuesError
  _ <- manyForParser parseSpace
  vals <- parseInsertValuesList
  _ <- manyForParser parseSpace
  _ <- parseChar ')'
  return vals

parseInsertValuesList :: Parser [String]
parseInsertValuesList = do
  val <- parseInsertValue
  vals <- manyForParser parseInsertValues
  return (val:vals)

parseInsertValue :: Parser String
parseInsertValue = do 
  inp <- lift get
  case inp of
    "" -> throwE _EMPTY_INPUT_ERROR
    (x:xs) ->
      if x == '\''
        then do 
          let out = '\'':takeWhile (/= '\'') xs
          _ <- trueOrThrowE (length out /= length inp) _ERROR_CLOSING_APOSTROPHE_MISSING
          lift $ put $ drop (length out +1) inp
          return $ out ++"\'"
        else parseInsertNameOrFun

parseInsertNameOrFun :: Parser String
parseInsertNameOrFun = do
  inp <- lift get 
  case runParser parseFunction inp of
    (Right f, out) -> do
      let fName = name f
      let col = column f
      let cName = insertForFuncResult col
      lift $ put out
      return $ fName ++ "("++ cName ++")"
    (Left _, _) -> parseName

insertForFuncResult :: Columns -> String
insertForFuncResult c =
  case c of
    AllC -> "*"
    NullC -> ""
    OneColumn name -> name

parseInsertValues :: Parser String
parseInsertValues = do
  _ <- manyForParser parseSpace
  _ <- parseChar ','
  _ <- manyForParser parseSpace
  parseInsertValue
--Insert parsing END

--Table names
parseRestShow :: Parser ParsedStatement
parseRestShow = do
  _ <- manyForParser parseSpace
  stat <- parseShowTableNames
  _ <- parseEnding
  return stat

parseShowTableNames :: Parser ParsedStatement
parseShowTableNames = fmap ParsedTables (parseAllT <:> parseTable) <:> showNoTablesError

parseAllT :: Parser Tables
parseAllT = (\_ -> AllT) <$> parseInsensitiveWord "TABLES"

parseTable :: Parser Tables
parseTable = do
  _ <- parseInsensitiveWord "TABLE" <:> showNoTableError
  _ <- manyForParser parseSpace
  fmap Table parseName
--Table names END

--Select statement setup
parseRestSelect :: Parser ParsedStatement
parseRestSelect = do
  _ <- manyForParser parseSpace
  pColumns <- parseColumnList <:> selectNoColumnsError
  _ <- manyForParser parseSpace
  _ <- parseInsensitiveWord "FROM" <:> syntaxErrorLikelySource "SELECT" "FROM"
  _ <- manyForParser parseSpace
  parseFromTable pColumns <:> parseFromTables pColumns 

parseFromTable :: Columns -> Parser ParsedStatement
parseFromTable cols = do
  tableName <- parseName 
  _ <- manyForParser parseSpace
  ps <- parseWhereOrderSemi
  return $ ParsedTable tableName cols (rowConditions ps) (orderBy ps)

parseFromTables :: Columns -> Parser ParsedStatement
parseFromTables cols = do
  tableName <- parseName <:> selectUnspecifiedTablesError
  tableNames <- manyForParser parseNames
  _ <- manyForParser parseSpace
  ps <- parseWhereOrderSemi
  return $ ManyTables (tableName:tableNames) cols (rowConditions ps) (orderBy ps)

parseWhereOrderSemi :: Parser ParsedStatement 
parseWhereOrderSemi = do
  inp <- lift get
  let whr = evalState (runExceptT parseWhereToStat) inp
  case whr of
    Right _ -> do 
      psW <- parseWhereToStat
      manyForParser parseSpace
      inp2 <- lift get
      let ord = evalState (runExceptT parseOrderToStat) inp2
      case ord of
        Right _ -> do
          psO <- parseOrderToStat
          return $ ParsedTable "" NullC (rowConditions psW) (orderBy psO) 
        Left _ -> do 
          _ <- parseEnding
          return $ ParsedTable "" NullC (rowConditions psW) NullOr
    Left _ -> do
      let ord = evalState (runExceptT parseOrderToStat) inp
      case ord of
        Right _ -> do
          psO <- parseOrderToStat
          return $ ParsedTable "" NullC NoConditions (orderBy psO) 
        Left _ -> do 
          _ <- parseEnding
          return $ ParsedTable "" NullC NoConditions NullOr

parseWhereToStat :: Parser ParsedStatement
parseWhereToStat = do
  cons <- parseWhere
  return $ ParsedTable "" NullC cons NullOr

parseOrderToStat :: Parser ParsedStatement
parseOrderToStat = do
  ord <- parseOrder
  parseEnding
  return $ ParsedTable "" NullC NoConditions ord
--Select statement setup END

--Column names
parseAllC :: Parser Columns
parseAllC = (\_ -> AllC) <$> parseChar '*'

parseSingleC :: Parser Columns
parseSingleC = do fmap OneColumn parseName

parseFunction :: Parser Columns
parseFunction = do
  fun <- parseName
  _ <- manyForParser parseSpace
  _ <- parseChar '('
  _ <- manyForParser parseSpace
  pColumn <- 
    parseAllC 
    <:> parseSingleC 
    <:> do
      _ <- parseOnlyUntil isSpace ')'
      return NullC 
    <:> unrecognisedFunInput
  _ <- manyForParser parseSpace
  _ <- parseChar ')'
  return $ Func {name = stringToLower fun, column = pColumn}

parseColumnList :: Parser Columns
parseColumnList = do
  col <- parseFunction <:> parseSingleC <:> parseAllC
  other <- manyForParser parseColumnsRest
  return $ ColumnList $ col:other

parseColumnsRest :: Parser Columns
parseColumnsRest = do
    _ <- manyForParser parseSpace
    _ <- parseChar ','
    _ <- manyForParser parseSpace
    parseFunction <:> parseSingleC <:> parseAllC
--Column names END

--Where statement check
parseConditions :: Parser Conditions
parseConditions = parseWhereEnding <:> do
  cond <- parseWhere
  parseEnding
  return cond

parseWhere :: Parser Conditions
parseWhere = do
  _ <- parseInsensitiveWord "WHERE" <:> unrecognisedCommandError
  _ <- manyForParser parseSpace
  cond <- parseConditionList <:> syntaxErrorLikelySource "WHERE" "the conditions"
  return cond

parseConditionList :: Parser Conditions
parseConditionList = do
  con <- parseCondition
  cons <- manyForParser parseConditionRest
  return $ Conditions $ con:cons

parseConditionRest :: Parser (String, String, String)
parseConditionRest = do
  _ <- manyForParser parseSpace
  _ <- parseInsensitiveWord "OR"
  _ <- manyForParser parseSpace
  parseCondition

parseCondition :: Parser (String, String, String)
parseCondition = do
  con1 <- parseName
  _ <- manyForParser parseSpace
  op <- parseOperator
  _ <- manyForParser parseSpace
  con2 <- parseName
  return (con1, op, con2)

parseWhereEnding :: Parser Conditions
parseWhereEnding = (\_ -> NoConditions) <$> parseEnding

parseOperator :: Parser String
parseOperator = manyForParser $ parseCaseChar isOperatorChar
--Where statement check END

--Order by statement
parseOrder :: Parser Order
parseOrder = do
  parseInsensitiveWord "ORDER" <:> throwE "EXPECTED ORDER, BUT NOT FOUND"
  manyForParser parseSpace
  parseInsensitiveWord "BY" <:> throwE "EXPECTED BY, BUT NOT FOUND"
  manyForParser parseSpace
  order <- parseSingleOrderColumn
  orders <- manyForParser parseRestOrderColumn
  return $ OrderList (order:orders)

parseRestOrderColumn :: Parser Order
parseRestOrderColumn = do
  manyForParser parseSpace
  parseChar ','
  manyForParser parseSpace
  parseSingleOrderColumn

parseSingleOrderColumn :: Parser Order
parseSingleOrderColumn = do
  col <- parseName
  manyForParser parseSpace
  aORd <- parseName
  case stringToLower aORd of
    "asc" -> return $ OrderOne col True
    "desc" -> return $ OrderOne col False
    _ -> throwE ""
--Order by statement END 

--General parsers 
parseCaseChar :: (Char -> Bool) -> Parser Char
parseCaseChar f = do
  inp <- lift get
  case inp of
    [] -> throwE _EMPTY_INPUT_ERROR
    (x:xs) -> if f x
      then do
        lift $ put xs
        return x
      else throwE _ERROR_CHAR_DID_NOT_MEET_FUNC

parseChar :: Char -> Parser Char
parseChar c = do
  inp <- lift get
  case inp of
    [] -> throwE _EMPTY_INPUT_ERROR
    (x:xs) -> if x == c
      then do
        lift $ put xs
        return c
      else throwE $ _ERROR_WAS_FOUND [c] [x]


parseWordWithCustomEquality :: (String -> String -> Bool) -> String -> Parser String
parseWordWithCustomEquality f s = do
  word <- parseName <:> nothingToMatchAgainst
  if f s word
    then return word
    else wordNotMatching s word

parseName :: Parser String
parseName = do 
  inp <- lift get
  case takeWhile validChar inp of
    "" -> throwE _EMPTY_INPUT_ERROR
    xs -> do
      lift $ put $ drop (length xs) inp
      return xs 

parseNames :: Parser String
parseNames = do
  _ <- manyForParser parseSpace
  _ <- parseChar ','
  _ <- manyForParser parseSpace
  parseName

parseEnding :: Parser Char
parseEnding = do
  _ <- parseOnlyUntil isSpace ';' <:> unrecognisedSymbolsAfterStatementError
  _ <- parseChar ';' <:> semicolonError
  _ <- manyForParser parseSpace
  return ';'

parseOnlyUntil :: (Char -> Bool) -> Char -> Parser Char 
parseOnlyUntil f c = do 
  inp <- lift get
  case inp of
    [] -> throwE _EMPTY_INPUT_ERROR
    (x:xs) -> if x == c
      then do
        lift $ put $ c:xs
        return c
      else if f x
        then do 
          lift $ put xs
          parseOnlyUntil f c
        else throwE _GENERAL_ERROR

parseSpace :: Parser Char
parseSpace = parseCaseChar isSpace

parseInsensitiveWord :: String -> Parser String
parseInsensitiveWord = parseWordWithCustomEquality caseInsensitiveEquality

parseNotSpaceString :: Parser String
parseNotSpaceString = do
  inp <- lift get
  case inp of
    "" -> throwE _EMPTY_INPUT_ERROR
    _ -> case takeWhile (not . isSpace) inp of
      "" -> throwE _ERROR_SPACES_IN_THE_WAY
      rez -> do
        lift $ put $ drop (length rez) inp
        return rez
--General parsers END
-----------------------------------------------------------------PARSER SETUP END