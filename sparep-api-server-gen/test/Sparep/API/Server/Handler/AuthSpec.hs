module Sparep.API.Server.Handler.AuthSpec (spec) where

import Network.HTTP.Types as HTTP
import Sparep.API
import Sparep.API.Data
import Sparep.API.Data.Gen ()
import Sparep.API.Server.TestUtils
import Sparep.Client
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = serverSpec $ do
  describe "PostRegister" $
    it "does not crash" $
      \cenv ->
        forAllValid $ \rf -> do
          NoContent <- testClientOrErr cenv $ postRegister sparepClient rf
          pure ()
  describe "PostRegister" $ do
    it "fails before registration" $ \cenv ->
      forAllValid $ \lf -> do
        errOrRes <- testClient cenv $ postLogin sparepClient lf
        case errOrRes of
          Left err -> case err of
            FailureResponse _ resp | responseStatusCode resp == HTTP.unauthorized401 -> pure ()
            _ -> failure $ "Should have errored with code 401, got this instead: " <> show err
          _ -> failure "Should have errored"
    it "shows no difference between a login failure for a user that exists and a user that doesn't exist" $ \cenv ->
      forAllValid $ \un1 ->
        forAll (genValid `suchThat` (/= un1)) $ \un2 ->
          forAllValid $ \pw1 ->
            forAll (genValid `suchThat` (/= pw1)) $ \pw2 -> do
              -- Sign up user 1 but not user 2
              NoContent <- testClientOrErr cenv $ postRegister sparepClient $ RegistrationForm {registrationFormUsername = un1, registrationFormPassword = pw1}
              errOrRes1 <- testClient cenv $ postLogin sparepClient $ LoginForm {loginFormUsername = un1, loginFormPassword = pw2}
              errOrRes2 <- testClient cenv $ postLogin sparepClient $ LoginForm {loginFormUsername = un2, loginFormPassword = pw2}
              () <$ errOrRes1 `shouldBe` () <$ errOrRes2
    it "succeeds after registration" $ \cenv ->
      forAllValid $ \rf -> do
        _ <- testClientOrErr cenv $ do
          NoContent <- postRegister sparepClient rf
          postLogin sparepClient $ registrationFormToLoginForm rf
        pure ()
