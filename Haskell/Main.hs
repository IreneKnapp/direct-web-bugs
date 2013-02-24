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
import qualified Data.UUID as UUID
import qualified Data.Word as Word
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Network.HTTP as HTTP
import qualified Network.Socket as Net
import qualified System.Environment as IO
import qualified System.Exit as IO

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
    Configuration <$> value JSON..: "process"
                  <*> value JSON..: "network"
                  <*> value JSON..: "files"
                  <*> value JSON..: "access"
                  <*> value JSON..: "service"
  parseJSON _ = mzero


data ProcessConfiguration =
  ProcessConfiguration {
      processConfigurationUser :: Maybe String,
      processConfigurationGroup :: Maybe String
    }
instance JSON.FromJSON ProcessConfiguration where
  parseJSON (JSON.Object value) =
    ProcessConfiguration <$> value JSON..:? "user"
                         <*> value JSON..:? "group"
  parseJSON _ = mzero


data NetworkConfiguration =
  NetworkConfiguration {
      networkConfigurationPort :: Int
    }
instance JSON.FromJSON NetworkConfiguration where
  parseJSON (JSON.Object value) =
    NetworkConfiguration <$> value JSON..: "port"
  parseJSON _ = mzero


data FilesConfiguration =
  FilesConfiguration {
      filesConfigurationDatabasePath :: FilePath
    }
instance JSON.FromJSON FilesConfiguration where
  parseJSON (JSON.Object value) =
    FilesConfiguration <$> value JSON..: "database"
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
    passwordSHA1 <- value JSON..:? "password-sha1" >>= decodeBlob
    AccessConfiguration <$> pure passwordSHA1
  parseJSON _ = mzero


data ServiceConfiguration =
  ServiceConfiguration {
      serviceConfigurationPath :: String
    }
instance JSON.FromJSON ServiceConfiguration where
  parseJSON (JSON.Object value) =
    ServiceConfiguration <$> value JSON..: "path"
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
          HTTP.acceptLoop serverParameters $ do
            request <- getRequest
            dispatchRequest request allHandlers
    _ -> do
      putStrLn $ "Usage: qmic configuration.json"
      IO.exitFailure


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
  -> RequestPattern
  -> RequestPattern
expectJSONContent name oldPattern =
  oldPattern {
      requestPatternContent = JSONRequestContentPattern name
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


dispatchRequest
  :: (HTTP.MonadHTTP m)
  => Request
  -> [(RequestPattern, Map.Map String Dynamic -> m ())]
  -> m ()
dispatchRequest request [] = do
  HTTP.httpLog $ "No request-pattern matched."
  HTTP.setResponseStatus 400
dispatchRequest request ((pattern, handler) : rest) = do
  case matchRequest request pattern of
    Right variables -> handler variables
    Left maybeMessage -> do
      case maybeMessage of
        Just message -> HTTP.httpLog message
        Nothing -> return ()
      dispatchRequest request rest


matchRequest
  :: Request
  -> RequestPattern
  -> Either (Maybe String) (Map.Map String Dynamic)
matchRequest request pattern = do
  if requestMethod request /= requestPatternMethod pattern
    then Left Nothing
    else do
      uriVariables <-
        foldM (\soFar (requestComponent, patternComponent) -> do
                 case patternComponent of
                   ConstantPathComponentPattern expectedComponent -> do
                     if requestComponent == expectedComponent
                       then return soFar
                       else Left Nothing
                   VariablePathComponentPattern name parser -> do
                     case parser requestComponent of
                       Nothing -> Left Nothing
                       Just value -> return $ Map.insert name value soFar)
             Map.empty
             (zip (requestPath request) (requestPatternPath pattern))
      if requestHasTrailingSlash request /= requestPatternTrailingSlash pattern
        then Left Nothing
        else do
          let patternQueryVariables = requestPatternQueryVariables pattern
              (expectedQueryVariables, unexpectedQueryVariables) =
                partition (\(key, value) ->
                              case Map.lookup key patternQueryVariables of
                                Just _ -> True
                                Nothing -> False)
                          (Map.toList $ requestQueryVariables request)
          case map fst unexpectedQueryVariables of
            [key] ->
              Left $ Just $ "Unexpected query variable " ++ key ++ "."
            keys@(_ : _) ->
              Left $ Just $ "Unexpected query variables "
                            ++ englishList keys
                            ++ "."
            [] -> do
              let queryVariables = Map.fromList expectedQueryVariables
                  missingQueryVariables =
                    Map.difference patternQueryVariables queryVariables
              case map fst $ Map.toList missingQueryVariables of
                [key] ->
                  Left $ Just $ "Missing query variable " ++ key ++ "."
                keys@(_ : _) ->
                  Left $ Just $ "Missing query variables "
                                ++ englishList keys
                                ++ "."
                [] -> do
                  let visit key value = do
                        case Map.lookup key patternQueryVariables of
                          Nothing -> Left Nothing
                          Just parser ->
                            case parser value of
                              Nothing -> Left Nothing
                              Just result -> return (key, result)
                  queryVariables <-
                    mapM (\(key, value) -> visit key value)
                         (Map.toList queryVariables)
                    >>= return . Map.fromList
                  return $ foldl' Map.union
                                  Map.empty
                                  [uriVariables, queryVariables]


englishList :: [String] -> String
englishList [] = "(none)"
englishList [item] = item
englishList [item1, item2] = item1 ++ " and " ++ item2
englishList items = intercalate ", " (init items ++ ["and " ++ last items])


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


allHandlers
  :: (HTTP.MonadHTTP m)
  => [(RequestPattern, Map.Map String Dynamic -> m ())]
allHandlers =
  let infixl 1 $>>
      infixl 2 >>>
      ($>>) :: RequestPattern
            -> (RequestPattern -> RequestPattern)
            -> RequestPattern
      ($>>) = flip ($)
      (>>>) :: (RequestPattern -> RequestPattern)
            -> (RequestPattern -> RequestPattern)
            -> (RequestPattern -> RequestPattern)
      (>>>) = flip (.)
      prefix :: RequestPattern -> RequestPattern
      prefix = expectConstantPathComponent "qmic"
      apiPrefix :: RequestPattern -> RequestPattern
      apiPrefix = prefix >>> expectConstantPathComponent "api"
      frontEndPrefix :: RequestPattern -> RequestPattern
      frontEndPrefix = prefix
  in [(simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "login"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       loginAPIHandler),
      
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "logout"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       logoutAPIHandler),
      
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "confirm"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       confirmAPIHandler),
      
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "account"
       >>> expectConstantPathComponent "email"
       >>> expectTrailingSlash
       >>> expectJSONContent "json",
       accountEmailListAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "account"
       >>> expectConstantPathComponent "email"
       >>> expectVariablePathComponent "email" emailParser
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       accountEmailAddAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "account"
       >>> expectConstantPathComponent "email"
       >>> expectVariablePathComponent "email" emailParser
       >>> expectConstantPathComponent "delete"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       accountEmailDeleteAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "account"
       >>> expectConstantPathComponent "email"
       >>> expectVariablePathComponent "email" emailParser
       >>> expectConstantPathComponent "primary"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       accountEmailSetPrimaryAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "player"
       >>> expectTrailingSlash
       >>> expectJSONContent "json",
       playerListAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "rule"
       >>> expectTrailingSlash
       >>> expectJSONContent "json",
       ruleListAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectTrailingSlash
       >>> expectJSONContent "json",
       proposalListAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectConstantPathComponent "vote"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       proposalVoteAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectConstantPathComponent "add"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       proposalAddAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectVariablePathComponent "proposal_id" uuidParser
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       proposalDetailsAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectVariablePathComponent "proposal_id" uuidParser
       >>> expectConstantPathComponent "update"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       proposalUpdateAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectVariablePathComponent "proposal_id" uuidParser
       >>> expectConstantPathComponent "delete"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       proposalDeleteAPIHandler),
       
      (simpleRequestPattern "POST" $>> apiPrefix
       >>> expectConstantPathComponent "proposal"
       >>> expectVariablePathComponent "proposal_id" uuidParser
       >>> expectConstantPathComponent "submit"
       >>> expectNoTrailingSlash
       >>> expectJSONContent "json",
       proposalSubmitAPIHandler),
       
      (simpleRequestPattern "GET" $>> frontEndPrefix
       >>> expectTrailingSlash
       >>> expectNoContent,
       frontEndHandler),
       
      (simpleRequestPattern "GET" $>> frontEndPrefix
       >>> expectConstantPathComponent "confirm"
       >>> expectNoTrailingSlash
       >>> expectQueryVariable "code" confirmationCodeParser
       >>> expectNoContent,
       frontEndConfirmHandler)]


emailParser :: String -> Maybe Dynamic
emailParser input = do
  return $ toDyn input


uuidParser :: String -> Maybe Dynamic
uuidParser input = do
  uuid <- case reads input of
            [(uuid, "")] -> return uuid
            _ -> Nothing
  return $ toDyn (uuid :: UUID.UUID)


confirmationCodeParser :: String -> Maybe Dynamic
confirmationCodeParser input =
  decodeConfirmationCode input >>= return . toDyn


encodeConfirmationCode :: Bool -> BS.ByteString -> Maybe String
encodeConfirmationCode spaced byteString = do
  let (string, _, _) =
        foldl' (\(soFar, sum, nBits) byte ->
                   let sum' = Bits.shiftL sum 8 + fromIntegral byte
                       encode n = confirmationCodeCharacters !! n
                       takeSome (soFar, sum, nBits) =
                         if nBits >= 5
                           then takeSome
                                  (soFar ++ [encode $
                                              Bits.shiftR sum (nBits - 5)],
                                   sum Bits..&.
                                     (Bits.shiftR 1 (nBits - 5) - 1),
                                   nBits - 5)
                           else (soFar, sum, nBits)
                   in takeSome (soFar, sum', nBits + 8))
               ("" :: String, 0 :: Int, 0 :: Int)
               (BS.unpack byteString)
  if spaced
    then do
      let group1 = take 4 string
          group2 = take 4 $ drop 4 string
          group3 = take 4 $ drop 8 string
          group4 = drop 12 string
          spaced = group1 ++ " " ++ group2 ++ " " ++ group3 ++ " " ++ group4
      return spaced
    else return string


decodeConfirmationCode :: String -> Maybe BS.ByteString
decodeConfirmationCode string = do
  let bitBundles =
        foldl' (\soFar c ->
                  case elemIndex (Char.toLower c) confirmationCodeCharacters of
                    Nothing -> soFar
                    Just bitBundle -> soFar ++ [bitBundle])
               []
               string
  if length bitBundles /= 16
    then Nothing
    else return ()
  let (bytes, _, _) =
        foldl' (\(bytes, sum, nBits) bitBundle ->
                   let sum' = Bits.shiftL sum 5 + bitBundle
                   in if nBits + 5 >= 8
                       then (bytes ++ [fromIntegral $ sum' Bits..&. 0xFF],
                             Bits.shiftR sum' 8,
                             nBits + 5 - 8)
                       else (bytes, sum', nBits + 5))
               ([] :: [Word.Word8], 0 :: Int, 0 :: Int)
               bitBundles
  return $ BS.pack bytes


confirmationCodeCharacters :: String
confirmationCodeCharacters = "23456789abcdefghjkmnpqrstuvwxyz"


loginAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
loginAPIHandler variables = do
  HTTP.httpPutStr $ "Login placeholder."


logoutAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
logoutAPIHandler variables = do
  HTTP.httpPutStr $ "Logout placeholder."


confirmAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
confirmAPIHandler variables = do
  HTTP.httpPutStr $ "Confirm placeholder."


accountEmailListAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
accountEmailListAPIHandler variables = do
  HTTP.httpPutStr $ "Account email list placeholder."


accountEmailAddAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
accountEmailAddAPIHandler variables = do
  HTTP.httpPutStr $ "Account email add placeholder."


accountEmailDeleteAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
accountEmailDeleteAPIHandler variables = do
  HTTP.httpPutStr $ "Account email delete placeholder."


accountEmailSetPrimaryAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
accountEmailSetPrimaryAPIHandler variables = do
  HTTP.httpPutStr $ "Account email set primary placeholder."


playerListAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
playerListAPIHandler variables = do
  HTTP.httpPutStr $ "Player list placeholder."


ruleListAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
ruleListAPIHandler variables = do
  HTTP.httpPutStr $ "Rule list placeholder."


proposalListAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalListAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal list placeholder."


proposalVoteAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalVoteAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal vote placeholder."


proposalAddAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalAddAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal add placeholder."


proposalDetailsAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalDetailsAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal details placeholder."


proposalUpdateAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalUpdateAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal update placeholder."


proposalDeleteAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalDeleteAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal delete placeholder."


proposalSubmitAPIHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
proposalSubmitAPIHandler variables = do
  HTTP.httpPutStr $ "Proposal submit placeholder."


frontEndHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
frontEndHandler variables = do
  HTTP.setResponseHeader HTTP.HttpTransferEncoding "chunked"
  HTTP.httpPutStr $ "Front-end placeholder."


frontEndConfirmHandler
  :: (HTTP.MonadHTTP m) => Map.Map String Dynamic -> m ()
frontEndConfirmHandler variables = do
  HTTP.setResponseHeader HTTP.HttpTransferEncoding "chunked"
  let maybeCode :: Maybe BS.ByteString
      maybeCode = Map.lookup "code" variables >>= fromDynamic
  case maybeCode of
    Nothing -> HTTP.httpCloseOutput
    Just code -> do
      HTTP.httpPutStr $ "Front-end confirm placeholder."
                        ++ "\n" ++ (show $ encodeConfirmationCode True code)
                        ++ "\n" ++ "http://ireneknapp.com/qmic/confirm?code="
                        ++ (show $ encodeConfirmationCode False code)
                        ++ "\n" ++ (show $ encodeConfirmationCode True
                                     $ BS.pack $ take 16 $ repeat 0xAA)

