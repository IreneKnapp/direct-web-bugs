{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.State.Strict as MTL
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified Data.Word as Word
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Network.HTTP as HTTP
import qualified Network.Socket as Net
import qualified System.Environment as IO
import qualified System.Exit as IO

import qualified Text.Blaze.Html as B

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad
import Data.Dynamic
import Data.List
import Data.Maybe


data Configuration =
  Configuration {
      configurationProcess :: ProcessConfiguration,
      configurationNetwork :: NetworkConfiguration,
      configurationFiles :: FilesConfiguration,
      configurationAccess :: AccessConfiguration,
      configurationService :: ServiceConfiguration
    }
instance JSON.FromJSON Configuration where
  parseJSON (JSON.Object value) =
    Configuration <$> value .: "process"
                  <*> value .: "network"
                  <*> value .: "files"
                  <*> value .: "access"
                  <*> value .: "service"
  parseJSON _ = mzero


data ProcessConfiguration =
  ProcessConfiguration {
      processConfigurationUser :: Maybe String,
      processConfigurationGroup :: Maybe String
    }
instance JSON.FromJSON ProcessConfiguration where
  parseJSON (JSON.Object value) =
    ProcessConfiguration <$> value .:? "user"
                         <*> value .:? "group"
  parseJSON _ = mzero


data NetworkConfiguration =
  NetworkConfiguration {
      networkConfigurationPort :: Int
    }
instance JSON.FromJSON NetworkConfiguration where
  parseJSON (JSON.Object value) =
    NetworkConfiguration <$> value .: "port"
  parseJSON _ = mzero


data FilesConfiguration =
  FilesConfiguration {
      filesConfigurationDatabasePath :: FilePath
    }
instance JSON.FromJSON FilesConfiguration where
  parseJSON (JSON.Object value) =
    FilesConfiguration <$> value .: "database"
  parseJSON _ = mzero


data AccessConfiguration =
  AccessConfiguration {
      accessConfigurationPasswordSHA1 :: Maybe BS.ByteString
    }
instance JSON.FromJSON AccessConfiguration where
  parseJSON (JSON.Object value) = do
    let decodeBlob maybeString = do
          case maybeString of
            Nothing -> return Nothing
            Just string -> do
              let nibbles = computeNibbles string
              if length nibbles /= 40
                then mzero
                else return $ Just $ BS.pack $ computeBytes nibbles
        computeNibbles string =
          foldl' (\soFar c ->
                    case elemIndex (Char.toLower c) "0123456789abcdef" of
                      Nothing -> soFar
                      Just nibble -> soFar ++ [fromIntegral nibble])
                 []
                 string
        computeBytes [] = []
        computeBytes (high : low : rest) =
          (Bits.shiftL high 4 Bits..|. low) : computeBytes rest
    passwordSHA1 <- value .:? "password-sha1" >>= decodeBlob
    AccessConfiguration <$> pure passwordSHA1
  parseJSON _ = mzero


data ServiceConfiguration =
  ServiceConfiguration {
      serviceConfigurationPath :: String
    }
instance JSON.FromJSON ServiceConfiguration where
  parseJSON (JSON.Object value) =
    ServiceConfiguration <$> value .: "path"
  parseJSON _ = mzero


data Request =
  Request {
      requestMethod :: String,
      requestPath :: [String],
      requestHasTrailingSlash :: Bool,
      requestQueryVariables :: Map.Map String String,
      requestContent :: RequestContent
    }
  deriving (Show)


data RequestContent
  = NoRequestContent
  | JSONRequestContent BS.ByteString
  | UnknownRequestContent
  deriving (Show)


data RequestPattern =
  RequestPattern {
      requestPatternMethod :: String,
      requestPatternPath :: [PathComponentPattern],
      requestPatternTrailingSlash :: Bool,
      requestPatternQueryVariables :: Map.Map String (String -> Maybe Dynamic),
      requestPatternContent :: RequestContentPattern
    }


data PathComponentPattern
  = ConstantPathComponentPattern String
  | VariablePathComponentPattern String (String -> Maybe Dynamic)


data RequestContentPattern
  = NoRequestContentPattern
  | JSONRequestContentPattern String


getServerParameters :: Configuration -> IO HTTP.HTTPServerParameters
getServerParameters configuration = do
  let portIn = fromIntegral $ networkConfigurationPort $
                configurationNetwork configuration
      port =
        (Bits.shiftL (portIn Bits..&. 0xFF) 8)
        Bits..|. (Bits.shiftR portIn 8 Bits..&. 0xFF)
      address = Net.SockAddrInet (Net.PortNum port) 0x0100007F
  return $
    HTTP.HTTPServerParameters {
        HTTP.serverParametersAccessLogPath = Nothing,
        HTTP.serverParametersErrorLogPath = Nothing,
        HTTP.serverParametersDaemonize =
          True,
        HTTP.serverParametersUserToChangeTo =
          processConfigurationUser $ configurationProcess configuration,
        HTTP.serverParametersGroupToChangeTo =
          processConfigurationGroup $ configurationProcess configuration,
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
          let expectedPath =
                serviceConfigurationPath $ configurationService configuration
          HTTP.acceptLoop serverParameters $ do
            path <- HTTP.getRequestURI
            method <- HTTP.getRequestMethod
            case method of
              "GET" | path == expectedPath -> handleGetRequest
              "POST" | path == expectedPath -> handlePostRequest
              _ -> HTTP.setResponseStatus 404
    _ -> do
      putStrLn $ "Usage: qmic configuration.json"
      IO.exitFailure


uuidParser :: String -> Maybe UUID.UUID
uuidParser input = do
  case reads input of
    [(uuid, "")] -> return uuid
    _ -> Nothing


handleGetRequest :: (HTTP.MonadHTTP m) => m ()
handleGetRequest = do
  HTTP.httpPutStr $ "GET placeholder."

handlePostRequest :: (HTTP.MonadHTTP m) => m ()
handleGetRequest = do
  HTTP.httpPutStr $ "POST placeholder."
