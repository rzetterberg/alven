module Unit.Foreign.Lua.UtilSpec (spec) where

import           TestImport 

import           Foreign.Lua.Util (findAPICalls)

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
