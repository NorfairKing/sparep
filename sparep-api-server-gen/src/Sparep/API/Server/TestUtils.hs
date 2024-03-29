{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.API.Server.TestUtils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp as Warp
import Servant.API
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client
import Sparep.API
import Sparep.API.Data
import Sparep.API.Data.Gen ()
import Sparep.API.Server
import Sparep.API.Server.Data
import Sparep.API.Server.Env
import Sparep.Client
import Test.QuickCheck
import Test.Syd hiding (HList (..))
import Test.Syd.Validity
import Web.Cookie

serverSpec :: SpecWith ClientEnv -> Spec
serverSpec =
  before (HTTP.newManager defaultManagerSettings) . aroundWith withTestServer
    . modifyMaxSuccess (`div` 20)

withTestServer :: (ClientEnv -> IO a) -> (HTTP.Manager -> IO a)
withTestServer func man =
  runNoLoggingT $
    withSqlitePool ":memory:" 1 $ \pool -> do
      void $ runSqlPool (runMigrationQuiet serverMigration) pool
      liftIO $ do
        jwk <- generateKey
        let serverEnv =
              Env
                { envConnectionPool = pool,
                  envCookieSettings = defaultCookieSettings,
                  envJWTSettings = defaultJWTSettings jwk
                }
        let serverApp = sparepAPIServerApp serverEnv
        testWithApplication (pure serverApp) $ \p -> do
          let env = mkClientEnv man $ BaseUrl Http "127.0.0.1" p ""
          func env

testClientOrErr :: ClientEnv -> ClientM a -> IO a
testClientOrErr cenv func = do
  res <- testClient cenv func
  case res of
    Left err -> failure $ show err
    Right r -> pure r

testClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClient = flip runClientM

registrationFormToLoginForm :: RegistrationForm -> LoginForm
registrationFormToLoginForm rf =
  LoginForm
    { loginFormUsername = registrationFormUsername rf,
      loginFormPassword = registrationFormPassword rf
    }

withAnyNewUser :: Testable a => ClientEnv -> (Token -> IO a) -> Property
withAnyNewUser cenv func = forAllValid $ \rf -> ioProperty $ withNewUser cenv rf func

withNewUser :: ClientEnv -> RegistrationForm -> (Token -> IO a) -> IO a
withNewUser cenv rf func = do
  testClientOrErr cenv $ do
    NoContent <- postRegister sparepClient rf
    pure ()
  token <- testLogin cenv $ registrationFormToLoginForm rf
  func token

testLogin :: ClientEnv -> LoginForm -> IO Token
testLogin cenv lf = do
  Headers NoContent (HCons sessionHeader HNil) <- testClientOrErr cenv $ postLogin sparepClient lf
  case sessionHeader of
    MissingHeader -> failure "The server responded but the response was missing the right session header."
    UndecodableHeader _ -> failure "The server responded but the response had an undecodable session header."
    Header setCookieText -> do
      let cookies = parseSetCookie . TE.encodeUtf8 <$> T.lines setCookieText
          jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
      case jwtCookie of
        Nothing -> failure "No JWT-Cookie was found in the Set-Cookie session header."
        Just setCookie -> pure $ Token $ setCookieValue setCookie

failure :: String -> IO a
failure = expectationFailure
