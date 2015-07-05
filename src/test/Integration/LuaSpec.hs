{-# OPTIONS_GHC -fno-warn-orphans #-}

module Integration.LuaSpec (spec) where

import qualified Data.Text as T
import           TestImport hiding (assertEqual)
import           Test.HUnit (assertEqual, assertFailure)

import qualified Foreign.Lua as Lua
import           Foreign.Lua.Types (LuaExtra(LuaExtra))

-------------------------------------------------------------------------------

spec :: Spec
spec = withApp $ describe "lua theme output" $ do
    it "outputs a simple 'hello'" $
        checkTheme "test/static/lua/hello" "hello"
    it "outputs page name from db" $ do
        let expOutp = T.unpack (textPageName tmpPage1)

        void $ runDB $ insert tmpPage1
    
        checkTheme "test/static/lua/page_name" expOutp
    it "outputs html page list from db using lustache template engine" $ do
        let expOutp = T.unpack $ pagesToHTMLList [tmpPage1, tmpPage2]

        runDB $ do
            void $ insert tmpPage1
            void $ insert tmpPage2
    
        checkTheme "test/static/lua/lustache_page_list" expOutp
  where
    tmpPage1  = TextPage "Test page 1" "test-page1" "" True Nothing
    tmpPage2  = TextPage "Test page 2" "test-page2" "" True Nothing
    checkTheme themeDir expOutp = do
        yesod        <- getTestYesod
        outputBuffer <- liftIO $ newIORef ""

        let urlRenderer _ = "nop"
            lextra        = LuaExtra themeDir "" (runDBIO yesod)
                                     outputBuffer urlRenderer

        liftIO $ do
            result <- Lua.runThemeScript lextra

            case result of
                Left errm  -> assertFailure errm
                Right outp -> assertEqual "theme expected result" expOutp outp

-------------------------------------------------------------------------------
-- * Utils

pagesToHTMLList :: [TextPage] -> Text
pagesToHTMLList ps = T.intercalate "" $ "<ul>" : li ++ ["</ul>", "\n"]
  where
    li     = map toLi ps
    toLi p = T.intercalate "" ["<li>", textPageName p, "</li>"]
