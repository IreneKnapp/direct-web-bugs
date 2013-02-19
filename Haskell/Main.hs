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


data Handler =
  Handler {
      handlerAction :: (MonadHTTP m) => m Bool
    }


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


allHandlers :: [Handler]
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
   proposalAddAPIHandler,
   proposalDeleteAPIHandler,
   proposalSubmitAPIHandler,
   proposalVoteAPIHandler,
   loginFrontEndHandler,
   logoutFrontEndHandler,
   accountFrontEndHandler,
   writeFrontEndHandler,
   voteFrontEndHandler,
   rulesFrontEndHandler,


/qmic/api/login?next=<href> POST
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
/qmic/api/proposal/add POST
/qmic/api/proposal/delete POST
/qmic/api/proposal/submit POST
/qmic/api/proposal/vote POST
/qmic/login?next=<href> GET
/qmic/logout GET POST
/qmic/account GET
/qmic/write GET
/qmic/vote GET
/qmic/rules GET
/qmic/proposals GET
/qmic/players GET
