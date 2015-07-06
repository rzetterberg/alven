{-# OPTIONS_GHC -fno-warn-orphans #-}

module Integration.LuaSpec (spec) where

import qualified Data.Text as T
import           TestImport hiding (assertEqual)
import           Test.HUnit (assertEqual, assertFailure)

import qualified Foreign.Lua as Lua
import           Foreign.Lua.Types (LuaExtra(LuaExtra))

-------------------------------------------------------------------------------

spec :: Spec
spec = withApp $ do
    describe "lua API interaction" $ do
        it "output returns expected result" $
            checkTheme "test/static/lua/api/output" "hello"
        it "get_theme_url returns expected result" $
            checkTheme "test/static/lua/api/get_theme_url" "nop"
        it "get_current_page gets right page" $ do
            let expOutp = T.unpack (textPageName tmpPage1)

            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            checkTheme "test/static/lua/api/get_current_page" expOutp
        it "get_pages returns expected amount of pages" $ do
            let expOutp = show (length [tmpPage1, tmpPage2])

            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            checkTheme "test/static/lua/api/get_pages" expOutp
        it "read_theme_file successfully reads a css file" $ do
            let expOutp = "body{color: red;}" :: Text

            liftIO $
                writeFile "test/static/lua/api/read_theme_file/main.css" expOutp

            checkTheme "test/static/lua/api/read_theme_file" (T.unpack expOutp)
    describe "lua common theme functionality" $ do
        it "lustache based page list" $ do
            let expOutp = T.unpack $ pagesToHTMLList [tmpPage1, tmpPage2]

            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            checkTheme "test/static/lua/examples/page_list" expOutp
  where
    tmpPage1  = TextPage "Test page 1" "test-page1" "" True Nothing
    tmpPage2  = TextPage "Test page 2" "test-page2" "" True Nothing
    checkTheme themeDir expOutp = do
        yesod        <- getTestYesod
        outputBuffer <- liftIO $ newIORef ""

        let urlRenderer _ = "nop"
            currPlink     = (textPagePermalink tmpPage1)
            lextra        = LuaExtra themeDir currPlink (runDBIO yesod)
                                     outputBuffer urlRenderer

        liftIO $ do
            result <- Lua.runThemeScript lextra

            case result of
                Left errm  -> assertFailure errm
                Right outp -> assertEqual "theme expected result" expOutp outp

-------------------------------------------------------------------------------
-- * Utils

{-|
Generates a HTML unordered list of pages without any spaces or line breaks.

>>> pagesToHTMLList [TextPage "Page 1" "page1" "" True Nothing]
"<ul><li>Page 1</li></ul>
-}
pagesToHTMLList :: [TextPage] -> Text
pagesToHTMLList ps = T.intercalate "" $ "<ul>" : li ++ ["</ul>", "\n"]
  where
    li     = map toLi ps
    toLi p = T.intercalate "" ["<li>", textPageName p, "</li>"]
