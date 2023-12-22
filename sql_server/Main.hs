{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Web.Scotty
import Data.Functor((<&>))
import Control.Monad.Free (Free (..))
import Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as TL
import Data.Aeson as AE (ToJSON, encode, decode)
import InMemoryTables
import DataFrame 
import Network.HTTP.Types.Status (Status, mkStatus)
import SQLServer 
import Data.IORef
import YamlHandler
import SQLCustomDataTypes
import Data.Time ( getCurrentTime )
import Functions.DFOperating (findTableByNameNoCase)
import Control.Monad
import Control.Concurrent
import System.Posix.Signals
import System.IO

jsonSerialization :: ToJSON a => a -> String
jsonSerialization = BS.unpack . encode

jsonStringDeserialization :: ByteString -> Maybe String
jsonStringDeserialization = AE.decode

changeInDb :: String -> DataFrame -> Database -> Database
changeInDb tName df db = changeInDb1 tName df db []

changeInDb1 :: String -> DataFrame -> Database -> Database -> Database
changeInDb1 _ _ [] dbTemp = dbTemp
changeInDb1 tName df (x:xs) dbTemp =
  if fst x == tName 
    then dbTemp++[(tName,df)]++xs
    else changeInDb1 tName df xs $ dbTemp++[x]

removeInDb :: String -> Database -> Database
removeInDb tName db = removeInDb1 tName db []

removeInDb1 :: String -> Database -> Database -> Database
removeInDb1 _ [] dbTemp = dbTemp
removeInDb1 tName (x:xs) dbTemp = 
  if fst x == tName
    then dbTemp++xs
    else removeInDb1 tName xs $ dbTemp++[x]
  
saveEverySecond :: MVar () -> IORef Database -> IO ()
saveEverySecond lock db = do
  takeMVar lock
  dbVal <- readIORef db
  saveDB dbVal
  putMVar lock ()
  threadDelay 1000000
  saveEverySecond lock db

main :: IO ()
main = do
  tid <- myThreadId
  db <- readDBWithTablesYAML
  lock <- newMVar ()
  case db of
    Left _ -> error "Server not able to start, because of an error while reading the database, please check if the database file is accessable and every table is too."
    Right rDb -> do
      memDB <- newIORef rDb
      forkIO $ saveEverySecond lock memDB
      installHandler sigINT (Catch $ beforeExit tid memDB lock) Nothing
      Prelude.putStrLn "Server starting"
      scotty 8888 $ do
        post "/" $ do
          val <- body
          liftIO $ takeMVar lock
          output <- liftIO $ runExecuteIO (executeSql $ BS.unpack val) memDB
          liftIO $ putMVar lock ()
          case output of
            Left msg -> do
              status badInpStatus
              text $ TL.pack msg
            Right df -> do
              if (isCreating $ BS.unpack val) || (isDropping $ BS.unpack val)
                then do
                  status noDfStatus              
                  if isCreating $ BS.unpack val
                  then text $ TL.pack $ "New table created successfully."
                  else text $ TL.pack $ "Table dropped successfully."
                else do
                  status dfStatus
                  text $ TL.pack $ jsonSerialization $ df

beforeExit :: ThreadId -> IORef Database -> MVar () -> IO ()
beforeExit id db lock = do
  dbVals <- readIORef db
  takeMVar lock
  Prelude.putStrLn "Turning server off."
  saveDB dbVals
  killThread id

runExecuteIO :: SQLServer.Execution r -> IORef Database-> IO r
runExecuteIO (Pure r) _ = return r
runExecuteIO (Free step) db = do
    next <- runStep step
    runExecuteIO next db
    where
        runStep :: SQLServer.ExecutionAlgebra a -> IO a
        runStep (SQLServer.GetTime next) = getCurrentTime >>= return . next
        runStep (SQLServer.LoadDatabase next) = do 
          dbVal <- readIORef db
          (return $ Right dbVal) >>= return . next
        runStep (SQLServer.WriteOutTable tName df next) = do
          dbNoRef <- readIORef db
          case findTableByNameNoCase dbNoRef tName of
            Nothing -> (writeIORef db $ dbNoRef++[(tName,df)]) >>= return . next
            Just _ -> (writeIORef db $ changeInDb tName df dbNoRef) >>= return . next
        runStep (SQLServer.RemoveTable tName next) = do
          dbNoRef <- readIORef db
          let newDbNoRef = removeInDb tName dbNoRef
          writeIORef db newDbNoRef >>= return . next 

dfStatus :: Status
dfStatus = mkStatus 201 "DataFrame returned."

noDfStatus :: Status
noDfStatus = mkStatus 202 "No DataFrame returned."

badInpStatus :: Status
badInpStatus = mkStatus 203 "Bad user input, database could not execute."