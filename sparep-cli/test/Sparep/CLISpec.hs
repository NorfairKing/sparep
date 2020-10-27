module Sparep.CLISpec (spec) where

import qualified Data.Text as T
import Path
import Path.IO
import Servant.Client
import Sparep.API.Data
import Sparep.API.Server.Data
import Sparep.API.Server.TestUtils
import Sparep.CLI
import System.Environment
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec
  $ describe "Sparep CLI"
  $ it "'just works'"
  $ \cenv -> forAllValid $ \rf -> withSystemTempDir "sparep-cli" $ \tdir -> do
    setEnv "SPAREP_SERVER_URL" $ showBaseUrl $ baseUrl cenv
    setEnv "SPAREP_USERNAME" $ T.unpack $ usernameText $ registrationFormUsername rf
    setEnv "SPAREP_PASSWORD" $ T.unpack $ registrationFormPassword rf
    dbFile <- resolveFile tdir "sparep-client.sqlite3"
    setEnv "SPAREP_DATABASE" $ fromAbsFile dbFile
    let testSparep args = withArgs args sparepCLI
    testSparep ["register"]
    testSparep ["login"]
    testSparep ["sync"]
    testSparep ["count"]
