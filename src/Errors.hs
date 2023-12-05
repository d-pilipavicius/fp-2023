{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Errors where 

--GENERAL ERRORS
_GENERAL_ERROR :: String
_GENERAL_ERROR = "Error"

_EMPTY_INPUT_ERROR :: String
_EMPTY_INPUT_ERROR = "Empty Input"

_ERROR_WAS_FOUND :: String -> String -> String
_ERROR_WAS_FOUND s1 s2 = "Expected " ++ s1 ++ " but " ++ s2 ++ " was found."

_ERROR_BAD_SYNTAX_AFTER :: String -> String -> String
_ERROR_BAD_SYNTAX_AFTER s1 s2 = "There was a syntax error after "++s1++", most likely with the spelling of "++s2++"."

_ERROR_UNSPECIFIED_TABLE :: String -> String
_ERROR_UNSPECIFIED_TABLE s = "Please specify a table name. For example: "++s++"."
--GENERAL ERRORS END

--Errors with no place yet (Executor)
_ERROR_STATEMENT_NOT_INIT :: String
_ERROR_STATEMENT_NOT_INIT = "ParsedStatement not initialized."

_ERROR_BAD_CONDITION_ARGUMENT :: String -> String
_ERROR_BAD_CONDITION_ARGUMENT s = "The provided argument "++s++" is neither a column, or a number, please provide a correct value."

_ERROR_NO_TABLE_IN_DATABASE :: String -> String
_ERROR_NO_TABLE_IN_DATABASE s = "Table " ++ s ++ " was not found in the database."

_ERROR_COLUMN_NOT_FOUND :: String -> String
_ERROR_COLUMN_NOT_FOUND s = "Column "++s++" was not found in the provided table(s)."

_ERROR_NOT_INTEGER_TYPE :: String -> String
_ERROR_NOT_INTEGER_TYPE s = "The provided column "++s++" is not of IntegerType."

_ERROR_PARSED_TABLE_NOT_INITIATED :: String
_ERROR_PARSED_TABLE_NOT_INITIATED = "The provided parsed table was initiated incorrectly, please make sure you are using parseStatement beforehand."

_ERROR_UNRECOGNISED_AGGREGATE :: String -> String
_ERROR_UNRECOGNISED_AGGREGATE s = "Unrecognised function \""++s++"\"."

_ERROR_UNRECOGNISED_OPERATOR :: String -> String
_ERROR_UNRECOGNISED_OPERATOR s = "Opperator "++s++" not recognised."

_ERROR_VALUE_NOT_INTEGER :: String 
_ERROR_VALUE_NOT_INTEGER = "This function supports IntegerValue only."

_ERROR_TABLE_ROW_NOT_MATCHING :: String
_ERROR_TABLE_ROW_NOT_MATCHING = "The provided tables have different amounts of rows in them and cannot be joined."

_ERROR_COULD_NOT_LOAD_DATABASE :: String
_ERROR_COULD_NOT_LOAD_DATABASE = "The database file cannot be accesed. Please check the database file and run the code again."

_ERROR_COULD_NOT_LOAD_TABLE :: String -> String
_ERROR_COULD_NOT_LOAD_TABLE s = "The info for table "++s++" could not be accessed. Please check if the exists in the database or if the file for the table exists."

_ERROR_FUNCTION_NOW_FOUND_PARAMETERS :: String
_ERROR_FUNCTION_NOW_FOUND_PARAMETERS = "The brackets in the function NOW() must be left empty inside."

_ERROR_FUNCTION_NO_ARGUMENT :: String -> String
_ERROR_FUNCTION_NO_ARGUMENT f = "The function "++f++" must have an argument inside."

_ERROR_TABLE_MULTIPLE_INSTANCES :: String
_ERROR_TABLE_MULTIPLE_INSTANCES = "Duplicate table names were found, please use unique tables."

_ERROR_AGGREGATE_FUNCTIONS :: String 
_ERROR_AGGREGATE_FUNCTIONS = "Columns and aggregate functions cannot be mixed."

_ERROR_NOT_A_VALUE :: String -> String
_ERROR_NOT_A_VALUE s = s++" is not a readable value."

_ERROR_EXECUTE_VALUE_DOES_NOT_ALIGN_WITH_TYPE :: String
_ERROR_EXECUTE_VALUE_DOES_NOT_ALIGN_WITH_TYPE = "The provided values cannot be entered, because one of the value types does not align with the column type."

_ERROR_EXECUTE_DUPLICATE_NAMES :: String
_ERROR_EXECUTE_DUPLICATE_NAMES = "Duplicate column names found in the statement."

_ERROR_EXECUTE_INSERT_NOT_ENOUGH_VALUES :: String
_ERROR_EXECUTE_INSERT_NOT_ENOUGH_VALUES = "When providing specific columns, please specify what you want to place in each column, or a placeholder null for no value."

_ERROR_EXECUTE_NOT_COLUMN_NAME :: String
_ERROR_EXECUTE_NOT_COLUMN_NAME = "When naming columns, only use column names, not * or any functions."
--Errors with no place yet END (Executor)

--SHOW errors
_ERROR_SHOW_NO_TABLES :: String
_ERROR_SHOW_NO_TABLES= "No table names were entered after SHOW. For example: SHOW tables;"

_ERROR_SHOW_NO_TABLE :: String
_ERROR_SHOW_NO_TABLE = "SHOW clause should be followed by a TABLE if a single table is to be selected."
--SHOW errors end

--SELECT errors
_ERROR_SELECT_NO_COLUMNS :: String
_ERROR_SELECT_NO_COLUMNS = "No column names were entered after SELECT. For example: SELECT * ..."

_ERROR_SELECT_UNSPECIFIED_TABLES :: String
_ERROR_SELECT_UNSPECIFIED_TABLES = "Expected table name or names after FROM. For example: \"table_name1, table_name2\""
--SELECT errors END

--UPDATE errors
_ERROR_UPDATE_UNSPECIFIED_ROWS :: String
_ERROR_UPDATE_UNSPECIFIED_ROWS = "Please specify which rows you want to update correctly. For example: \"row_name = value\"."
--UPDATE errors END

--DELETE errors
--DELETE errors END

--INSERT errors
_ERROR_INSERT_MISSING_VALUES :: String
_ERROR_INSERT_MISSING_VALUES = "Added values are missing from the statement. For example \"VALUES (val1, val2)\""

_ERROR_INSERT_INTO_SYNTAX :: String
_ERROR_INSERT_INTO_SYNTAX = "There was a syntax error after INSERT INTO table_name."
--INSERT errors END

--Specific errors for parsers
_ERROR_BAD_FIRST_COMMAND_2 :: String -> String
_ERROR_BAD_FIRST_COMMAND_2 s = "Unrecognised command \""++s++"\", please use SELECT, SHOW, UPDATE, DELETE, INSERT."

_ERROR_MISSING_SEMICOLON :: String
_ERROR_MISSING_SEMICOLON = "Expected ; was not found. A statement must end with a ; For example: SELECT * FROM table;"

_ERROR_BAD_STATEMENT_1 :: String
_ERROR_BAD_STATEMENT_1 = "Expected ; or a WHERE clause after the remaining statement. For example: \"SELECT * FROM table_name;\" OR \"DELETE FROM table_name WHERE conditions;\""

_ERROR_CHARACTERS_AFTER_SEMICOLON :: String
_ERROR_CHARACTERS_AFTER_SEMICOLON = "The space after ; should be left empty."

_ERROR_CHAR_DID_NOT_MEET_FUNC :: String
_ERROR_CHAR_DID_NOT_MEET_FUNC = "Error char not in function range."

_ERROR_SPACES_IN_THE_WAY :: String
_ERROR_SPACES_IN_THE_WAY = "Unable to parse from space characters."

_ERROR_UNRECOGNISED_SYMBOLS :: String
_ERROR_UNRECOGNISED_SYMBOLS = "Either the statement does not contain a ;, or unrecognised symbols were found after the last statement."

_ERROR_UNRECOGNISED_SYMBOLS_IN_FUNCTION :: String
_ERROR_UNRECOGNISED_SYMBOLS_IN_FUNCTION = "Unrecognised symbols found in a function, please check the provided arguments inside of functions."

_ERROR_NOTHING_TO_MATCH_AGAINST :: String
_ERROR_NOTHING_TO_MATCH_AGAINST = "The comparer cannot extract a word from the given input."

_ERROR_CLOSING_APOSTROPHE_MISSING :: String
_ERROR_CLOSING_APOSTROPHE_MISSING = "When inserting values, the single apostrophe was never closed."
--Specific errors for parsers END 