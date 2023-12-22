{-# LANGUAGE DeriveFunctor #-}
module Lib3
  ( executeSql,
    Execution,
    ExecutionAlgebra(..),
    insertTime
  )
where

import Lib2
import Control.Monad.Free (Free (..), liftF)
import Functions.DFOperating
import DataFrame (DataFrame, Value (StringValue))
import Data.Time ( UTCTime )
import CustomDataTypes ( ErrorMessage, ParsedStatement(..))
import GeneralConstants (_TIME_INSERT_VALUE)

type TableName = String

data ExecutionAlgebra next
  = LoadDatabase (Either ErrorMessage Database -> next)
  | GetTime (UTCTime -> next)
  | WriteOutTable TableName DataFrame (() -> next)
  | ExecuteLib2 (Either ErrorMessage Database) UTCTime Bool String (Either ErrorMessage (TableName, DataFrame) -> next)
  -- feel free to add more constructors here
  deriving Functor

type Execution = Free ExecutionAlgebra

getTime :: Execution UTCTime
getTime = liftF $ GetTime id

readDatabase :: Execution (Either ErrorMessage Database)
readDatabase = liftF $ LoadDatabase id 

writeOutTable :: TableName -> DataFrame -> Execution ()
writeOutTable tName df = liftF $ WriteOutTable tName df id

insertTime :: UTCTime -> DataFrame -> Either ErrorMessage DataFrame
insertTime t = changeAllColumnValuesTo (StringValue _TIME_INSERT_VALUE) (StringValue $ show t)

executeSql :: String -> Execution (Either ErrorMessage DataFrame)
executeSql sql = do
  db <- readDatabase
  let boolVal = isUpdatingDb sql
  time <- getTime
  df <- return $ do
    db1 <- db
    ps <- parseStatement sql
    df <- executeStatement db1 ps
    df1 <- insertTime time df
    return (table ps, df1)
  case df of
    Left l -> return $ Left l
    Right r ->
      if boolVal
        then do
          _ <- writeOutTable (fst r) (snd r)
          return $ Right $ snd r
        else return $ Right $ snd r
