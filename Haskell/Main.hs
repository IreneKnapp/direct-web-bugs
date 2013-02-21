{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.State.Strict as MTL
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Network.HTTP as HTTP
import qualified Network.Socket as Net
import qualified System.Environment as IO
import qualified System.Exit as IO

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.List
import Data.Maybe


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
  | JSONRequestContentPattern String (BS.ByteString -> Maybe Dynamic)


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
            request <- getRequest
            HTTP.httpPutStr $ "'" ++ show request ++ "'"
    _ -> do
      putStrLn $ "Usage: qmic configuration.json"
      IO.exitFailure


{-
allHandlers :: (HTTP.MonadHTTP m) => [m Bool]
allHandlers =
  [loginAPIHandler,
   logoutAPIHandler,
   confirmAPIHandler,
   accountEmailListAPIHandler,
   accountEmailDetailsAPIHandler,
   accountEmailAddAPIHandler,
   accountEmailDeleteAPIHandler,
   accountEmailSetPrimaryAPIHandler,
   playerListAPIHandler,
   playerDetailsAPIHandler,
   ruleListAPIHandler,
   ruleDetailsAPIHandler,
   proposalListAPIHandler,
   proposalVoteAPIHandler,
   proposalAddAPIHandler,
   proposalDetailsAPIHandler,
   proposalUpdateAPIHandler,
   proposalDeleteAPIHandler,
   proposalSubmitAPIHandler,
   loginFrontEndHandler,
   logoutFrontEndHandler,
   accountFrontEndHandler,
   writeFrontEndHandler,
   voteFrontEndHandler,
   rulesFrontEndHandler,
   proposalsFrontEndHandler,
   playersFrontEndHandler]
   -}


simpleRequestPattern :: String -> RequestPattern
simpleRequestPattern method =
  RequestPattern {
      requestPatternMethod = method,
      requestPatternPath = [],
      requestPatternTrailingSlash = True,
      requestPatternQueryVariables = Map.empty,
      requestPatternContent = NoRequestContentPattern
    }


expectConstantPathComponent :: String -> RequestPattern -> RequestPattern
expectConstantPathComponent component oldPattern =
  let oldPath = requestPatternPath oldPattern
      newPath = oldPath ++ [ConstantPathComponentPattern component]
  in oldPattern {
         requestPatternPath = newPath
       }


expectVariablePathComponent
  :: String -> (String -> Maybe Dynamic) -> RequestPattern -> RequestPattern
expectVariablePathComponent name parser oldPattern =
  let oldPath = requestPatternPath oldPattern
      newPath = oldPath ++ [VariablePathComponentPattern name parser]
  in oldPattern {
         requestPatternPath = newPath
       }


expectTrailingSlash :: RequestPattern -> RequestPattern
expectTrailingSlash oldPattern =
  oldPattern {
      requestPatternTrailingSlash = True
    }


expectNoTrailingSlash :: RequestPattern -> RequestPattern
expectNoTrailingSlash oldPattern =
  oldPattern {
      requestPatternTrailingSlash = False
    }


expectQueryVariable
  :: String -> (String -> Maybe Dynamic) -> RequestPattern -> RequestPattern
expectQueryVariable name parser oldPattern =
  let oldMap = requestPatternQueryVariables oldPattern
      newMap = Map.insert name parser oldMap
  in oldPattern {
         requestPatternQueryVariables = newMap
       }


expectNoContent :: RequestPattern -> RequestPattern
expectNoContent oldPattern =
  oldPattern {
      requestPatternContent = NoRequestContentPattern
    }


expectJSONContent
  :: String
  -> (BS.ByteString -> Maybe Dynamic)
  -> RequestPattern
  -> RequestPattern
expectJSONContent name parser oldPattern =
  oldPattern {
      requestPatternContent = JSONRequestContentPattern name parser
    }


getRequest :: (HTTP.MonadHTTP m) => m Request
getRequest = do
  method <- HTTP.getRequestMethod
  uri <- HTTP.getRequestURI
  let (pathString, query) = break (\c -> c == '?') uri
      pathString' = fromMaybe pathString $ stripPrefix "/" pathString
      (pathString'', hasTrailingSlash) =
        if isSuffixOf "/" pathString'
          then (take (length pathString' - 1) pathString', True)
          else (pathString', False)
      path = wordsLoop [] pathString''
      wordsLoop soFar string =
        case break (\c -> c == '/') string of
          (_, "") -> soFar ++ [string]
          (next, rest) -> wordsLoop (soFar ++ [next]) (tail rest)
      query' = fromMaybe query $ stripPrefix "?" query
      queryVariables = decodeFormVariables query'
  return Request {
             requestMethod = method,
             requestPath = path,
             requestHasTrailingSlash = hasTrailingSlash,
             requestQueryVariables = queryVariables,
             requestContent = NoRequestContent
           }


decodeFormVariables :: String -> Map.Map String String
decodeFormVariables input =
  let result =
        Map.fromList
         $ concatMap (\word ->
                        let (key, value) = break (\c -> c == '=') word
                        in case (unescape "" key,
                                 unescape "" $ drop 1 value) of
                             (Just key, Just value) -> [(key, value)]
                             _ -> [])
                     $ filter (\word -> length word > 0)
                              $ wordsLoop [] input
      wordsLoop soFar string =
        case break (\c -> c == '&') string of
          (_, "") -> soFar ++ [string]
          (next, rest) -> wordsLoop (soFar ++ [next]) (tail rest)
      unescape soFar [] = Just soFar
      unescape soFar ('%' : high : low : rest) = 
        let hexValue c =
              if Char.isDigit c
                then Just $ Char.ord c - Char.ord '0'
                else if Char.isAlpha c
                       then let value = Char.ord (Char.toLower c)
                                        - Char.ord 'a' + 10
                            in if value < 16
                                 then Just value
                                 else Nothing
                       else Nothing
        in case (hexValue high, hexValue low) of
             (Just high, Just low) | high < 8 ->
               unescape (soFar ++ [Char.chr (high * 16 + low)]) rest
             _ -> Nothing
      unescape soFar ('%' : _) = Nothing
      unescape soFar ('+' : rest) = unescape (soFar ++ " ") rest
      unescape soFar (c : rest) = unescape (soFar ++ [c]) rest
  in result


{-
whenRequestPattern
  :: (HTTP.MonadHTTP m)
  => RequestPattern
  -> (Map String Dynamic -> m Bool)
  -> m Bool
whenRequestPattern pattern handler = do
  method <- HTTP.getRequestMethod
  if method /= requestPatternMethod pattern
    then return False
    else do
      uri <- HTTP.getRequestURI


loginAPIHandler :: (HTTP.MonadHTTP m) => m Bool
loginAPIHandler =
  whenRequestPattern
    (RequestPattern {
         requestPatternMethod = "GET,
         requestPatternPath =
           uriPrefix
           ++ [ConstantPathComponentPattern "api",
               ConstantPathComponentPattern "login"]
         requestPatternIsDirectory = False,
         requestPatternQueryVariables =
           Map.fromList [("next", absoluteLocalURIDecoder)],
         requestPatternFormVariables =
           Map.fromList [("username", nonemptyTextDecoder),
                         ("password", nonemptyTextDecoder)]
       })
    $ \variables -> do
       httpLog "Login attempt"


logoutAPIHandler :: (MonadHTTP m) => m Bool
confirmAPIHandler :: (MonadHTTP m) => m Bool
accountEmailListAPIHandler :: (MonadHTTP m) => m Bool
accountEmailDetailsAPIHandler :: (MonadHTTP m) => m Bool
accountEmailAddAPIHandler :: (MonadHTTP m) => m Bool
accountEmailDeleteAPIHandler :: (MonadHTTP m) => m Bool
accountEmailSetPrimaryAPIHandler :: (MonadHTTP m) => m Bool
playerListAPIHandler :: (MonadHTTP m) => m Bool
playerDetailsAPIHandler :: (MonadHTTP m) => m Bool
ruleListAPIHandler :: (MonadHTTP m) => m Bool
ruleDetailsAPIHandler :: (MonadHTTP m) => m Bool
proposalListAPIHandler :: (MonadHTTP m) => m Bool
proposalVoteAPIHandler :: (MonadHTTP m) => m Bool
proposalAddAPIHandler :: (MonadHTTP m) => m Bool
proposalDetailsAPIHandler :: (MonadHTTP m) => m Bool
proposalUpdateAPIHandler :: (MonadHTTP m) => m Bool
proposalDeleteAPIHandler :: (MonadHTTP m) => m Bool
proposalSubmitAPIHandler :: (MonadHTTP m) => m Bool
loginFrontEndHandler :: (MonadHTTP m) => m Bool
logoutFrontEndHandler :: (MonadHTTP m) => m Bool
accountFrontEndHandler :: (MonadHTTP m) => m Bool
writeFrontEndHandler :: (MonadHTTP m) => m Bool
voteFrontEndHandler :: (MonadHTTP m) => m Bool
rulesFrontEndHandler :: (MonadHTTP m) => m Bool
proposalsFrontEndHandler :: (MonadHTTP m) => m Bool
playersFrontEndHandler :: (MonadHTTP m) => m Bool


{-
/qmic/api/login POST
/qmic/api/logout POST
/qmic/api/confirm?code=<code> GET
/qmic/api/account/email/ GET
/qmic/api/account/email/<email> GET
/qmic/api/account/email/<email>/delete POST
/qmic/api/account/email/<email>/primary POST
/qmic/api/player/ GET
/qmic/api/player/<id> GET
/qmic/api/rule/ GET
/qmic/api/rule/<id> GET
/qmic/api/proposal/ GET
/qmic/api/proposal/active/ GET
/qmic/api/proposal/passed/ GET
/qmic/api/proposal/failed/ GET
/qmic/api/proposal/vote POST
/qmic/api/proposal/add POST
/qmic/api/proposal/<id> GET POST
/qmic/api/proposal/<id>/delete POST
/qmic/api/proposal/<id>/submit POST
/qmic/ GET
-}
-}
