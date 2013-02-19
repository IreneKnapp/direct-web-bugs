{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.State.Strict as MTL
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Network.HTTP as HTTP
import qualified Network.Socket as Net
import qualified System.Environment as IO
import qualified System.Exit as IO

import Control.Applicative
import Control.Monad


data Configuration =
  Configuration {
      configurationDatabasePath :: FilePath,
      configurationPort :: Int,
      configurationAccessLogPath :: Maybe FilePath,
      configurationErrorLogPath :: Maybe FilePath,
      configurationUser :: Maybe String,
      configurationGroup :: Maybe String
    }
instance JSON.FromJSON Configuration where
  parseJSON (JSON.Object value) =
    Configuration <$> value JSON..: "database"
                  <*> value JSON..: "port"
                  <*> value JSON..:? "access-log"
                  <*> value JSON..:? "error-log"
                  <*> value JSON..:? "user"
                  <*> value JSON..:? "group"
  parseJSON _ = mzero


getServerParameters :: Configuration -> IO HTTP.HTTPServerParameters
getServerParameters configuration = do
  let port =
        (Bits.shiftL ((fromIntegral $ configurationPort configuration)
                      Bits..&. 0xFF) 8)
        Bits..|. (Bits.shiftR (fromIntegral $ configurationPort configuration)
                              8 Bits..&. 0xFF)
      address = Net.SockAddrInet (Net.PortNum port) 0x0100007F
  return $
    HTTP.HTTPServerParameters {
        HTTP.serverParametersAccessLogPath =
          configurationAccessLogPath configuration,
        HTTP.serverParametersErrorLogPath =
          configurationErrorLogPath configuration,
        HTTP.serverParametersDaemonize =
          True,
        HTTP.serverParametersUserToChangeTo =
          configurationUser configuration,
        HTTP.serverParametersGroupToChangeTo =
          configurationGroup configuration,
        HTTP.serverParametersForkPrimitive =
          Concurrent.forkIO,
        HTTP.serverParametersListenSockets =
          [HTTP.HTTPListenSocketParameters {
                HTTP.listenSocketParametersAddress = address,
                HTTP.listenSocketParametersSecure = False
             }]
      }


main :: IO ()
main = do
  arguments <- IO.getArgs
  case arguments of
    [configurationFilePath] -> do
      configurationJSON <- BS.readFile configurationFilePath
      case JSON.eitherDecode' (LBS.fromChunks [configurationJSON]) of
        Left message -> do
          putStrLn message
          IO.exitFailure
        Right configuration -> do
          serverParameters <- getServerParameters configuration
          HTTP.acceptLoop serverParameters $ do
            HTTP.httpLog "Hmm..."
    _ -> do
      putStrLn $ "Usage: qmic configuration.json"
      IO.exitFailure
