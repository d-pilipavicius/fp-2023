{-# LANGUAGE DeriveFunctor #-}
module Lib3
  ( executeSql,
    Execution,
    ExecutionAlgebra(..),
    insertTime,
    getCurrentTime,
    exportFun
  )
where

import Lib2
import Control.Monad.Free (Free (..), liftF)
import Functions.DFOperating
import DataFrame (DataFrame, Value (StringValue))
import Data.Time ( UTCTime, getCurrentTime)
import CustomDataTypes ( ErrorMessage, ParsedStatement(..))
import YamlHandler ( writeDFYAML, readDBWithTablesYAML, readDFYAML )
import GeneralConstants (_TIME_INSERT_VALUE)

type TableName = String
type FileContent = String
type Statement = String

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

executeLib2 :: Either ErrorMessage Database -> UTCTime -> Bool -> String -> Execution (Either ErrorMessage (TableName, DataFrame))
executeLib2 db time boolVal st = liftF $ ExecuteLib2 db time boolVal st id 

executeSql :: String -> Execution (Either ErrorMessage DataFrame)
executeSql sql = do
  db <- readDatabase
  let boolVal = isUpdatingDb sql
  time <- getTime
  df <- executeLib2 db time boolVal sql
  case df of
    Left l -> return $ Left l
    Right r ->
      if boolVal
        then do
          _ <- writeOutTable (fst r) (snd r)
          return $ Right $ snd r
        else return $ Right $ snd r


--REMOVE THIS LATER
runExecuteIO :: Execution r -> IO r
runExecuteIO (Pure r) = return r
runExecuteIO (Free step) = do
    next <- runStep step
    runExecuteIO next
    where
        runStep :: ExecutionAlgebra a -> IO a
        runStep (GetTime next) = getCurrentTime >>= return . next
        runStep (LoadDatabase next) = readDBWithTablesYAML >>= return . next
        runStep (WriteOutTable tName df next) = putStrLn tName >>= return . next
        runStep (ExecuteLib2 db time boolVal st next) = (return $ do
          db1 <- db
          ps <- parseStatement st
          df <- executeStatement db1 ps
          df1 <- insertTime time df
          return (table ps, df1)) >>= return . next

exportFun :: String -> IO String
exportFun s = do 
  df <- runExecuteIO $ executeSql s
  return $ show df 
