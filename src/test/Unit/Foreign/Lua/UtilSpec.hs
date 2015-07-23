module Unit.Foreign.Lua.UtilSpec (spec) where

import           TestImport 

import           Foreign.Lua.Types
import           Foreign.Lua.Util 

-------------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "findAPICalls" $ do
        it "single call clean" $ do
            (findAPICalls "alven.output(\"hello\")") `shouldBe` ["output"]
        it "multiple calls clean" $ do
            let inp = "alven.output(\"hello\")" ++
                      "alven.get_theme_url(\"logotype.png\")"
            
            (findAPICalls inp) `shouldBe` ["output", "get_theme_url"]
        it "single call dirty" $ do
            let inp = "as dqwd alven.output(\"hello\") asd"
            
            (findAPICalls inp) `shouldBe` ["output"]
        it "multiple calls dirty" $ do
            let inp = "asdas alven.output(\"hello\")q2dasd" ++
                      "alven.get_theme_url(\"logotype.png\") asd"
            
            (findAPICalls inp) `shouldBe` ["output", "get_theme_url"]
    describe "getExportName" $ do
        it "existing export" $ do
            (getExportName existsExp) `shouldBe` "exists"
        it "renamed export" $ do
            (getExportName renamedExp) `shouldBe` "renamed"
        it "removed export" $ do
            (getExportName removedExp) `shouldBe` "removed"
    describe "getExportVersion" $ do
        it "existing export" $ do
            (getExportVersion existsExp) `shouldBe` (1, 0)
        it "renamed export" $ do
            (getExportVersion renamedExp) `shouldBe` (2, 0)
        it "removed export" $ do
            (getExportVersion removedExp) `shouldBe` (3, 0)
    describe "getExportVersionLit" $ do
        it "existing export" $ do
            (getExportVersionLit existsExp) `shouldBe` "1.0"
        it "renamed export" $ do
            (getExportVersionLit renamedExp) `shouldBe` "2.0"
        it "removed export" $ do
            (getExportVersionLit removedExp) `shouldBe` "3.0"
    describe "getExportByName" $ do
        it "known names" $ do
            (getExportByName "exists" allExps) `shouldBe` (Just existsExp)
            (getExportByName "renamed" allExps) `shouldBe` (Just renamedExp)
            (getExportByName "removed" allExps) `shouldBe` (Just removedExp)
        it "unknown names" $ do
            (getExportByName "unknown name" allExps) `shouldBe` Nothing
            (getExportByName "asdasd21" allExps) `shouldBe` Nothing
            (getExportByName "exists1" allExps) `shouldBe` Nothing
  where
    existsExp  = Exists "exists" (1, 0) (\_ _ -> return 0)
    renamedExp = Renamed "renamed" "renamed_new" (2, 0)
    removedExp = Removed "removed" (3, 0)
    allExps    = [existsExp, renamedExp, removedExp]
