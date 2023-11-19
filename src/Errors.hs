{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Errors where 

_GENERAL_ERROR :: String
_GENERAL_ERROR = "Error"

_EMPTY_INPUT_ERROR :: String
_EMPTY_INPUT_ERROR = "Empty Input"

_ERROR_BAD_FIRST_COMMAND :: String
_ERROR_BAD_FIRST_COMMAND = "Unrecognised first command, please use commands SELECT ... or SHOW ..."

_ERROR_WAS_FOUND :: String -> String -> String
_ERROR_WAS_FOUND s1 s2 = "Expected " ++ s1 ++ " but " ++ s2 ++ " was found."

_ERROR_NO_TABLES_AFTER_SHOW :: String
_ERROR_NO_TABLES_AFTER_SHOW = "No table names were entered after SHOW. For example: SHOW tables;"

_ERROR_NO_COLUMNS_AFTER_SELECT :: String
_ERROR_NO_COLUMNS_AFTER_SELECT = "No column names were entered after SELECT. For example: SELECT * ..."

_ERROR_CHAR_DID_NOT_MEET_FUNC :: String
_ERROR_CHAR_DID_NOT_MEET_FUNC = "Error char not in function range."

_ERROR_MISSING_SEMICOLON :: String
_ERROR_MISSING_SEMICOLON = "Expected ; was not found. A statement must end with a ; For example: SELECT * FROM table;"

_ERROR_CHARACTERS_AFTER_SEMICOLON :: String
_ERROR_CHARACTERS_AFTER_SEMICOLON = "The space after ; should be left empty."

_ERROR_UNRECOGNISED_COMMAND_AFTER_FROM :: String
_ERROR_UNRECOGNISED_COMMAND_AFTER_FROM = "Expected ; or a WHERE clause after table name, check if the statement is correct."

_ERROR_NO_TABLE_CLAUSE :: String
_ERROR_NO_TABLE_CLAUSE = "SHOW clause should be followed by a TABLE if a single table is to be selected."

_ERROR_UNRECOGNISED_SYMBOLS :: String
_ERROR_UNRECOGNISED_SYMBOLS = "Either the statement does not contain a ;, or unrecognised symbols were found after the last statement."

_ERROR_STATEMENT_NOT_INIT :: String
_ERROR_STATEMENT_NOT_INIT = "ParsedStatement not initialized"

_ERROR_BAD_CONDITION_ARGUMENT :: String -> String
_ERROR_BAD_CONDITION_ARGUMENT s = "The provided argument "++s++" is neither a column, or a number, please provide a correct value."

_ERROR_NO_TABLE_IN_DATABASE :: String -> String
_ERROR_NO_TABLE_IN_DATABASE s = "Table " ++ s ++ " was not found in the database."

_ERROR_COLUMN_NOT_FOUND :: String -> String
_ERROR_COLUMN_NOT_FOUND s = "Column "++s++" was not found in the provided table."

_ERROR_NOT_INTEGER_TYPE :: String -> String
_ERROR_NOT_INTEGER_TYPE s = "The provided column "++s++" is not of IntegerType."

_ERROR_PARSED_TABLE_NOT_INITIATED :: String
_ERROR_PARSED_TABLE_NOT_INITIATED = "The provided parsed table was initiated incorrectly, please make sure you are using parseStatement beforehand."

_ERROR_UNRECOGNISED_AGGREGATE :: String -> String
_ERROR_UNRECOGNISED_AGGREGATE s = "Unrecognised function "++s

_ERROR_UNRECOGNISED_OPERATOR :: String -> String
_ERROR_UNRECOGNISED_OPERATOR s = "Opperator "++s++" not recognised."

_ERROR_VALUE_NOT_INTEGER :: String 
_ERROR_VALUE_NOT_INTEGER = "This function supports IntegerValue only."