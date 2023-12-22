{-# LANGUAGE DeriveFunctor #-}
module SQLServer where

import Control.Monad.Free (Free (..), liftF)
import Functions.DFOperating
import Data.Time ( UTCTime )
import SQLCustomDataTypes
import DataFrame 
import SQLExecutorImplementation
import GeneralConstants ( _TIME_INSERT_VALUE )
import SQLParserImplementation 

type TableName = String

data ExecutionAlgebra next
  = LoadDatabase (Either ErrorMessage Database -> next)
  | GetTime (UTCTime -> next)
  | WriteOutTable TableName DataFrame (() -> next)
  | RemoveTable TableName (() -> next)
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

removeTable :: TableName -> Execution () 
removeTable tName = liftF $ RemoveTable tName id

executeSql :: String -> Execution (Either ErrorMessage DataFrame)
executeSql sql = do
  db <- readDatabase
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
      if isUpdatingDb sql || isCreating sql 
        then do
          _ <- writeOutTable (fst r) (snd r)
          return $ Right $ snd r
        else if isDropping sql
          then do
            _ <- removeTable $ fst r
            return $ Right $ DataFrame [] []  
          else return $ Right $ snd r

isUpdatingDb :: String -> Bool
isUpdatingDb str =
  case parseStatement str of
    Left _ -> False
    Right ps ->
      case ps of
        UpdateTable {} -> True
        InsertInto {} -> True
        DeleteTableElements {} -> True
        _ -> False

isCreating :: String -> Bool
isCreating str = 
  case parseStatement str of
    Left _ -> False
    Right ps ->
      case ps of
        CreateTable {} -> True
        _ -> False

isDropping :: String -> Bool
isDropping str =
  case parseStatement str of
    Left _ -> False
    Right ps ->
      case ps of
        DropTable {} -> True
        _ -> False 