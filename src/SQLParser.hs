module SQLParser (
  Parser,
  runParser
) where

import CustomDataTypes
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State.Strict (State, runState)


type Parser a = ExceptT ErrorMessage (State String) a

runParser :: Parser a -> String -> (Either ErrorMessage a, String)
runParser p = runState (runExceptT p)