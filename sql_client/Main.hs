{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Free (Free (..))

import Data.Functor((<&>))
import Data.List qualified as L
import Lib1 qualified
import CustomDataTypes qualified as CDTS
import DataFrame qualified as DF
import Network.Wreq
import Control.Lens 
import Errors (_GENERAL_ERROR)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson as AE (decode, encode, FromJSON, ToJSON)
import Network.HTTP.Types.Status as HTS (statusCode)
import Control.Exception (try, SomeException)
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Console.Terminal.Size (Window, size, width)


instance FromJSON DF.Value
instance FromJSON DF.Column
instance FromJSON DF.DataFrame
instance FromJSON DF.ColumnType

jsonStringSerialization :: String -> String
jsonStringSerialization = BS.unpack . AE.encode

jsonDFDeserilization :: String -> Maybe DF.DataFrame
jsonDFDeserilization = AE.decode . BS.pack

jsonStringDeserialization :: String -> Maybe String
jsonStringDeserialization = AE.decode . BS.pack

type Repl a = HaskelineT IO a

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to SQL Client! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer n = do
  let names = [
              "select", "*", "from", "show", "table",
              "tables", "insert", "into", "values",
              "set", "update", "delete", "create", 
              "drop", "order", "by"
              ]
  return $ Prelude.filter (L.isPrefixOf n) names

cmd :: String -> Repl ()
cmd c = do
  s <- terminalWidth <$> liftIO size
  result <- liftIO $ cmd' s
  case result of
    Left err -> liftIO $ putStrLn $ "Error: " ++ err
    Right table -> liftIO $ putStrLn table
  where
    terminalWidth :: (Integral n) => Maybe (Window n) -> n
    terminalWidth = maybe 80 width
    cmd' :: Integer -> IO (Either String String)
    cmd' s = do
      output <- clientPost c
      renderOutput output s

main :: IO ()
main = do
  evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final

clientPost :: String -> IO (String, Int)
clientPost input = do
    let opt = defaults & header "Content-Type" .~ ["text/plain"]
    result <- try $ postWith opt "http://localhost:8888" (BS.pack input) :: IO (Either SomeException (Response BS.ByteString))
    case result of 
        Left _ -> return ("Unable to access server, server is down.", 503)
        Right response -> do
            let respStat = HTS.statusCode $ response ^. responseStatus
            let respStr = BS.unpack $ response ^. responseBody 
            return (respStr, respStat)

renderOutput :: (String, Int) -> Integer -> IO (Either String String)
renderOutput (str,stat) len = do
  case stat of
    201 -> 
      case jsonDFDeserilization str of
        Nothing -> return $ Left "Bad JSON format submitted from server."
        Just df -> return $ Right $ Lib1.renderDataFrameAsTable len df
    202 -> return $ Right str
    203 -> return $ Left str
    503 -> return $ Left str
    _ -> return $ Left _GENERAL_ERROR