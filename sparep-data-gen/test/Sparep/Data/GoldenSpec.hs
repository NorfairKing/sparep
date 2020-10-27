module Sparep.Data.GoldenSpec (spec) where

import Control.Monad
import Path
import Path.IO
import Sparep.Data
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  fs <- runIO $ do
    resourcesDir <- resolveDir' "test_resources"
    snd <$> listDirRecur resourcesDir
  forM_ fs $ \f ->
    describe (fromAbsFile f) $ do
      it "parses" $ do
        deck <- readRootedDeckOrDie f
        shouldBeValid deck
      it "resolves" $ do
        deck <- readRootedDeckOrDie f
        studyUnits <- resolveRootedDeck deck
        shouldBeValid studyUnits
