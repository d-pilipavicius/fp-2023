{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib1
  ( parseSelectAllStatement,
    findTableByName,
    validateDataFrame,
    renderDataFrameAsTable,
		isRowAndColumnTypeMatching,
		getType,
		matchValueToType
  )
where

import DataFrame (DataFrame (..), Row, Column (..), ColumnType (..), Value (..))
import InMemoryTables (TableName)

import CustomDataTypes
import Data.Char (toUpper, toLower)
type Database = [(TableName, DataFrame)]

-- Your code modifications go below this comment

-- 1) implement the function which returns a data frame by its name
-- in provided Database list
findTableByName :: Database -> String -> Maybe DataFrame
findTableByName [] _ = Nothing
findTableByName _ "" = Nothing
findTableByName (x:xs) n = 
	if stringToLower (getTableName x) == stringToLower n then Just (getDataFrame x) else findTableByName xs n

getTableName :: (TableName, DataFrame) -> TableName
getTableName (x,_) = x

getDataFrame :: (TableName, DataFrame) -> DataFrame
getDataFrame (_,xs) = xs

stringToLower :: String -> String
stringToLower a = stringToLowerNoStack a []

stringToLowerNoStack :: String -> String -> String
stringToLowerNoStack [] a = a
stringToLowerNoStack (x:xs) a = stringToLowerNoStack xs (a++[toLower x])

-- 1st end

-- 2) implement the function which parses a "select * from ..."
-- sql statement and extracts a table name from the statement
parseSelectAllStatement :: String -> Either ErrorMessage TableName
parseSelectAllStatement q = 
	case parseSelect q of
	Left x -> Left x
	Right q1 -> case parseSpace q1 of 
		Left x -> Left x
		Right q2 -> case parseStar q2 of
			Left x -> Left x
			Right q3 -> case parseSpace q3 of
				Left x -> Left x
				Right q4 -> case parseFrom q4 of
					Left x -> Left x
					Right q5 -> case parseSpace q5 of
						Left x -> Left x
						Right q6 -> case getName q6 of
							Left x -> Left x
							Right q7 -> Right q7

parseSelect :: String -> Either ErrorMessage String
parseSelect s1 =
	case parseChar 's' s1 of
	Left x -> Left x
	Right e1 -> case parseChar 'e' e1 of
		Left x -> Left x
		Right l1 -> case parseChar 'l' l1 of
			Left x -> Left x
			Right e2 -> case parseChar 'e' e2 of
				Left x -> Left x
				Right c1 -> case parseChar 'c' c1 of
					Left x -> Left x
					Right t1 -> case parseChar 't' t1 of
						Left x -> Left x
						Right e -> Right e
						
parseFrom :: String -> Either ErrorMessage String
parseFrom f1 =
	case parseChar 'f' f1 of
	Left x -> Left x
	Right r1 -> case parseChar 'r' r1 of
		Left x -> Left x
		Right o1 -> case parseChar 'o' o1 of
			Left x -> Left x
			Right m1 -> case parseChar 'm' m1 of
				Left x -> Left x
				Right e -> Right e
				
getName :: String -> Either ErrorMessage String
getName "" = Left "Empty String"
getName ";" = Left "No name provided"
getName a = parseSemicolon a

parseSemicolon :: String -> Either ErrorMessage String
parseSemicolon a = parseSemicolonNoStack a []

parseSemicolonNoStack :: String -> String -> Either ErrorMessage String
parseSemicolonNoStack "" _ = Left "Empty String"
parseSemicolonNoStack ";" a = Right a
parseSemicolonNoStack (x:xs) a = parseSemicolonNoStack xs (a++[x])

parseSpace :: String -> Either ErrorMessage String
parseSpace sp = parseChar ' ' sp
	
parseStar :: String -> Either ErrorMessage String
parseStar st = parseChar '*' st

parseChar :: Char -> String -> Either ErrorMessage String
parseChar _ [] = Left "Empty String"
parseChar c (x:xs) =
	if toLower c == x || toUpper c == x then Right xs else Left ([x] ++ " was found instead of " ++ [c])
	
-- 2nd end

-- 3) implement the function which validates tables: checks if
-- columns match value types, if rows sizes match columns,..
validateDataFrame :: DataFrame -> Either ErrorMessage ()
validateDataFrame df = 
	if not (areRowLengthsLikeColumn (dfRows df) (dfCol df)) then Left "The provided dataframe has incorrect table dimensions" else 
		if (areRowsAndColumnTypesMatching (dfRows df) (dfCol df)) then Right () else Left "The provided dataframe's rows have value types that don't match"

dfCol :: DataFrame -> [Column]
dfCol (DataFrame c _) = c

dfRows :: DataFrame -> [Row]
dfRows (DataFrame _ r) = r

columnElementCount :: [Column] -> Integer
columnElementCount c = toInteger(length c)

rowElementCount :: Row -> Integer
rowElementCount r = toInteger (length r)

isRowLengthLikeColumn :: Row -> [Column] -> Bool
isRowLengthLikeColumn r c = 
	if (rowElementCount r) == (columnElementCount c) then True else False
	
areRowLengthsLikeColumn :: [Row] -> [Column] -> Bool
areRowLengthsLikeColumn [] c = True
areRowLengthsLikeColumn (x:xs) c = 
	if isRowLengthLikeColumn x c then areRowLengthsLikeColumn xs c else False

isRowAndColumnTypeMatching :: Row -> [Column] -> Bool
isRowAndColumnTypeMatching [] [] = True
isRowAndColumnTypeMatching (x1:xs1) (x2:xs2) =
	if (x1 == NullValue) || ((matchValueToType x1) == (Just (getType x2))) then (isRowAndColumnTypeMatching xs1 xs2) else False
	
areRowsAndColumnTypesMatching :: [Row] -> [Column] -> Bool
areRowsAndColumnTypesMatching [] _ = True
areRowsAndColumnTypesMatching (x:xs) c =
	if isRowAndColumnTypeMatching x c then areRowsAndColumnTypesMatching xs c else False

matchValueToType :: Value -> Maybe ColumnType
matchValueToType (IntegerValue _) = Just IntegerType
matchValueToType (StringValue _) = Just StringType
matchValueToType (BoolValue _) = Just BoolType
matchValueToType _ = Nothing

getType :: Column -> ColumnType
getType (Column _ a) = a

--3rd end

-- 4) implement the function which renders a given data frame
-- as ascii-art table (use your imagination, there is no "correct"
-- answer for this task!), it should respect terminal
-- width (in chars, provided as the first argument)
renderDataFrameAsTable :: Integer -> DataFrame -> String
renderDataFrameAsTable l df = (renderCol l df) ++ (createMinusString l) ++ (renderRows l df)

renderCol :: Integer -> DataFrame -> String
renderCol l df = renderColEl (calcLength l (columnElementCount (dfCol df))) (dfCol df) (columnElementCount (dfCol df)) ""

calcLength :: Integer -> Integer -> Integer
calcLength l c = div ((l - c) + 1) c

rowCount :: [Row] -> Integer
rowCount r = toInteger (length r)

renderColEl :: Integer -> [Column] -> Integer -> String -> String
renderColEl l (x:xs) i rez =
	if i > 1 then renderColEl l xs (i-1) (rez ++ (createStringLengthC x l) ++ "|") else (rez ++ (createStringLengthC x 0) ++ "\n")
	
createStringLengthC :: Column -> Integer -> String
createStringLengthC (Column n _) 0 = n
createStringLengthC (Column n _) l = n ++ createWhiteSpaceString (l-(toInteger(length n)))

createWhiteSpaceString :: Integer -> String
createWhiteSpaceString i = [' ' | _ <- [1 .. i]]

createMinusString :: Integer -> String
createMinusString i = ['-' | _ <- [1 .. i]] ++ "\n"

renderRows :: Integer -> DataFrame -> String
renderRows l df = renderRowsEl (calcLength l (columnElementCount (dfCol df))) (dfRows df) (rowCount (dfRows df)) ""

renderRowsEl :: Integer -> [Row] -> Integer -> String -> String
renderRowsEl l [] _ _ = createWhiteSpaceString l
renderRowsEl l (x:xs) i rez = 
	if i > 1 then renderRowsEl l xs (i-1) (rez ++ (renderRow l x (rowElementCount x) "") ++ "\n") else (rez ++ (renderRow l x (rowElementCount x) ""))

renderRow :: Integer -> Row -> Integer -> String -> String
renderRow l (x:xs) i rez =
	if i > 1 then renderRow l xs (i-1) (rez ++ (createStringLengthH (getValue x) l) ++ "|") else (rez ++ (createStringLengthH (getValue x) 0))
	
getValue :: Value -> String
getValue (IntegerValue x) = show x
getValue (BoolValue x) = show x
getValue (StringValue x) = x
getValue NullValue = ""

createStringLengthH :: String -> Integer -> String
createStringLengthH v 0 = v
createStringLengthH v l = v ++ createWhiteSpaceString (l - toInteger (length v))
--4th end