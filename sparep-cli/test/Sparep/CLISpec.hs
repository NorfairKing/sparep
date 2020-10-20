module Sparep.CLISpec (spec) where

import qualified Data.Text as T
import Servant.Client
import Sparep.API.Data
import Sparep.API.Server.TestUtils
import Sparep.CLI
import Sparep.Server.Data
import System.Environment
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec
  $ describe "Sparep CLI"
  $ it "'just works'"
  $ \cenv -> forAllValid $ \rf -> withNewUser cenv rf $ \token -> do
    setEnv "SPAREP_BASE_URL" $ showBaseUrl $ baseUrl cenv
    setEnv "SPAREP_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
    setEnv "SPAREP_PASSWORD" $ T.unpack $ registrationFormPassword rf
    let testSparep args = withArgs args sparepCLI
    testSparep ["register"]
    testSparep ["login"]
    testSparep ["sync"]
