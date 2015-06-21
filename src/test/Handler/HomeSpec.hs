module Handler.HomeSpec (spec) where

import           TestImport

-------------------------------------------------------------------------------

spec :: Spec
spec = withApp $ do
    it "loads the index and checks that it needs authorization" $ do
        get HomeR
        statusIs 303
