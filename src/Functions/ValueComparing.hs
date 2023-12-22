module Functions.ValueComparing (
    caseInsensitiveEquality,
    stringToInt,
    stringToBool,
    maxString,
    validChar,
    isOperatorChar,
    maybeToEither,
    stringToLower,
    trueOrError
) where

import Errors ( _GENERAL_ERROR )
import CustomDataTypes ( ErrorMessage )
import Data.Char(toLower, isAlphaNum)
import Text.Read (readMaybe)

caseInsensitiveEquality :: String -> String -> Bool
caseInsensitiveEquality str1 str2 = (map toLower str1) == (map toLower str2)

stringToInt :: String -> Either ErrorMessage Integer
stringToInt s = maybeToEither $ readMaybe s

stringToBool :: String -> Either ErrorMessage Bool
stringToBool s = maybeToEither $ readMaybe s

maxString :: String -> String -> String
maxString s1 s2 = if s1 > s2 then s1 else s2

validChar :: Char -> Bool
validChar a = a == '_' || a == '.' || a == '-' || isAlphaNum a

isOperatorChar :: Char -> Bool
isOperatorChar c = c >= '<' && c <= '>'

maybeToEither :: Maybe a -> Either ErrorMessage a
maybeToEither el = 
  case el of
    Just a -> Right a
    Nothing -> Left _GENERAL_ERROR

stringToLower :: String -> String
stringToLower s = stringToLower1 s ""

stringToLower1 :: String -> String -> String
stringToLower1 [] l = l 
stringToLower1 (x:xs) l = stringToLower1 xs (l++[(toLower x)])

trueOrError :: Bool -> ErrorMessage -> Either ErrorMessage ()
trueOrError f msg =
  if f
    then Right ()
    else Left msg