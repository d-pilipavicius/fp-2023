module ParserConstants where

import Errors
import Parser

errorTemplate :: String -> Parser a
errorTemplate a = Parser $ \_ -> Left a

--General parser errors
generalError :: Parser a
generalError = errorTemplate _GENERAL_ERROR

syntaxErrorLikelySource :: String -> String -> Parser a
syntaxErrorLikelySource s1 s2 = errorTemplate $ _ERROR_BAD_SYNTAX_AFTER s1 s2

wordNotMatching :: String -> String -> Parser a
wordNotMatching s word = errorTemplate $ _ERROR_WAS_FOUND ("*" ++ s ++ "*") ("*" ++ word ++ "*")
--General parser errors END

--Specific parser errors
unrecognisedCommandError :: Parser a
unrecognisedCommandError = errorTemplate _ERROR_BAD_STATEMENT_1

semicolonError :: Parser a
semicolonError = errorTemplate _ERROR_MISSING_SEMICOLON

unrecognisedSymbolsAfterStatementError :: Parser a
unrecognisedSymbolsAfterStatementError = errorTemplate _ERROR_UNRECOGNISED_SYMBOLS

unrecognisedCommandError2 :: String -> Parser a
unrecognisedCommandError2 = errorTemplate . _ERROR_BAD_FIRST_COMMAND_2

unrecognisedFunInput :: Parser a
unrecognisedFunInput = errorTemplate _ERROR_UNRECOGNISED_SYMBOLS_IN_FUNCTION

nothingToMatchAgainst :: Parser a
nothingToMatchAgainst = errorTemplate _ERROR_NOTHING_TO_MATCH_AGAINST
--Specific parser errors END

--SHOW parser errors
showNoTablesError :: Parser a
showNoTablesError = errorTemplate _ERROR_SHOW_NO_TABLES

showNoTableError :: Parser a
showNoTableError = errorTemplate _ERROR_SHOW_NO_TABLE
--SHOW parser errors END

--SELECT parser errors
selectNoColumnsError :: Parser a
selectNoColumnsError = errorTemplate _ERROR_SELECT_NO_COLUMNS

selectUnspecifiedTableError :: Parser a
selectUnspecifiedTableError = errorTemplate $ _ERROR_UNSPECIFIED_TABLE "\"UPDATE table_name;\""

selectUnspecifiedTablesError :: Parser a
selectUnspecifiedTablesError = errorTemplate _ERROR_SELECT_UNSPECIFIED_TABLES
--SELECT parser errors END

--UPDATE parser errors
updateUnspecifiedRowsError :: Parser a
updateUnspecifiedRowsError = errorTemplate _ERROR_UPDATE_UNSPECIFIED_ROWS
--UPDATE parser errors END

--DELETE parser errors
deleteUnspecifiedTableError :: Parser a
deleteUnspecifiedTableError = errorTemplate $ _ERROR_UNSPECIFIED_TABLE "\"DELETE FROM table_name;\""
--DELETE parser errors END

--INSERT parser errors
insertUnspecifiedTableError :: Parser a 
insertUnspecifiedTableError = errorTemplate $ _ERROR_UNSPECIFIED_TABLE "\"INSERT INTO table_name ...;\""

insertMissingValuesError :: Parser a
insertMissingValuesError = errorTemplate _ERROR_INSERT_MISSING_VALUES 
--INSERT parser errors END