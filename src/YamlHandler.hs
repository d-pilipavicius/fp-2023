{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module YamlHandler(
    readDFYAML,
    readDBYAML,
    writeDFYAML,
    readDBWithTablesYAML
  )
where 

import DataFrame (Column (..), ColumnType (..), Row(..), Value (..), DataFrame (..))
import Data.Yaml (encodeFile, decodeFileEither, FromJSON, ToJSON)
import CustomDataTypes (ErrorMessage)
import Functions.ListOperating
import Errors (_ERROR_COULD_NOT_LOAD_DATABASE, _ERROR_COULD_NOT_LOAD_TABLE)
import Data.List.Split (splitOn)
import System.Directory
import System.FilePath

instance FromJSON Value
instance FromJSON Column
instance FromJSON DataFrame
instance FromJSON ColumnType

instance ToJSON Value
instance ToJSON Column
instance ToJSON DataFrame
instance ToJSON ColumnType

data DFExpr 
  = OTable DFExpr DFExpr
  | OColumns [DFExpr]
  | ORows [DFExpr]
  | ORow [DFExpr]
  | OColumn String ColumnType
  | OValue Value
  deriving Show

render :: DFExpr -> String
render (OValue (IntegerValue i)) = concat ["contents: ",show i,"\ntag: IntegerValue\n"]
render (OValue (StringValue s)) = concat ["contents: ",s,"\ntag: StringValue\n"]
render (OValue (BoolValue b)) = concat ["contents: ",show b,"\ntag: BoolValue\n"]
render (OValue NullValue) = "tag: NullValue\n"
render (OColumn s ct) = concat ["- ",s,"\n- ",show ct,"\n"]
render (ORow l) = concatMap ("- "++) $ map render l
render (ORows []) = "- []\n"
render (ORows l) = addTabs $ "- " ++ (concatMap ("- "++) $ map render l)
render (OColumns l) = addTabs $ "- " ++ (concatMap ("- "++) $ map render l)
render (OTable c r) = render c ++ render r
  
addTabs :: String -> String
addTabs s = do
  let asList = splitOn "\n" s
  let correct = map (++"\n") $ map countAndAdd asList
  concat $ removeLastElement correct

countAndAdd :: String -> String
countAndAdd s = do
  let parsed = take 6 s
  let count = length (filter (== '-') parsed)
  let tabs = replicate (3-count) "  "
  concat tabs++s

class ToDFExpr a where
  toDFExpr :: a -> DFExpr

instance ToDFExpr DataFrame where
  toDFExpr (DataFrame c r) = OTable (OColumns $ map toDFExpr c) (ORows $ map (\x -> ORow $ map toDFExpr x) r)

instance ToDFExpr Column where
  toDFExpr (Column name cType) = OColumn name cType

instance ToDFExpr Value where
  toDFExpr = OValue 

--Constants
defaultDbDir :: IO String
defaultDbDir = do
  dir <- getCurrentDirectory 
  list <- listDirectory dir
  if or $ map (=="src") list 
    then return $ "src" </> "db"
    else return "db"

dbInfoName :: String
dbInfoName = "database"

dbInfoPath :: IO String
dbInfoPath = do 
  path <- defaultDbDir
  return $ path </> (dbInfoName ++ ".yaml")
--Constants END

readDFYAML :: String -> IO (Either ErrorMessage DataFrame)
readDFYAML tableName = do 
  dir <- defaultDbDir
  df <- decodeFileEither (dir </> (tableName ++ ".yaml"))
  case df of 
    Left _ -> return $ Left $ _ERROR_COULD_NOT_LOAD_TABLE tableName
    Right r -> return $ Right r 

writeDFYAML :: String -> DataFrame -> IO ()
writeDFYAML tName df = do
  dir <- defaultDbDir
  writeFile (dir </> (tName++".yaml")) $ dfToYAMLFileOutput df

dfToYAMLFileOutput :: DataFrame -> String
dfToYAMLFileOutput df = render $ toDFExpr df

readDBYAML :: IO (Either ErrorMessage [String])
readDBYAML = do
  dir <- dbInfoPath
  db <- decodeFileEither dir
  case db of
    Left _ -> return $ Left _ERROR_COULD_NOT_LOAD_DATABASE
    Right r -> return $ Right r

--THIS SHOULD BE USED ONLY TO CHANGE THE DATABASE BY HAND
writeDBYAML :: [String] -> IO ()
writeDBYAML strs = do
  dir <- dbInfoPath
  encodeFile dir strs
--THIS SHOULD BE USED ONLY TO CHANGE THE DATABASE BY HAND END

readDBWithTablesYAML :: IO (Either ErrorMessage [(String, DataFrame)])
readDBWithTablesYAML = do
  names <- readDBYAML
  case names of 
    Left l -> return $ Left l
    Right r -> do
      tuples <- traverse readTupleYAML r
      return $ sequence tuples

readTupleYAML :: String -> IO (Either ErrorMessage (String, DataFrame))
readTupleYAML tName = do
  df <- readDFYAML tName
  case df of
    Left l -> return $ Left l
    Right r -> return $ Right (tName, r)