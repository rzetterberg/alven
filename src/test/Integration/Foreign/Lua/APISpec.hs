{-# OPTIONS_GHC -fno-warn-orphans #-}

module Integration.Foreign.Lua.APISpec (spec) where

import qualified Data.Text as T
import           TestImport hiding (assertEqual)
import           Test.HUnit (assertEqual, assertFailure)

import qualified Foreign.Lua as Lua
import           Foreign.Lua.Types (LuaExtra(LuaExtra))

-------------------------------------------------------------------------------

spec :: Spec
spec = withApp $ do
    describe "alven.output" $ do
        it "single output" $
            checkTheme "api/output_single" "hello"
        it "multiple output" $
            checkTheme "api/output_multiple" "hellohello2"
    describe "alven.get_theme_url" $ do
        it "fake setup" $
            checkTheme "api/get_theme_url" "nop"
    describe "alven.get_current_page" $ do
        it "public page is retrieved" $ do
            let expOutp = T.unpack (textPageName tmpPage1)

            runDB $ do
                void $ insert tmpPage1

            checkTheme "api/get_current_page" expOutp
        it "private page is not retrieved" $ do
            runDB $ do
                void $ insert tmpPage1{ textPagePublic = False }

            checkTheme "api/get_current_page" ""
    describe "alven.get_pages" $ do
        it "1 public and 1 private returns 1" $ do
            let expOutp = "1" 

            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2{ textPagePublic = False }

            checkTheme "api/get_pages" expOutp
    describe "alven.read_theme_file" $ do
        it "read_theme_file successfully reads a css file" $ do
            let expOutp = "body{color: red;}" :: Text

            liftIO $
                writeFile "test/static/lua/api/read_theme_file/main.css" expOutp

            checkTheme "api/read_theme_file" (T.unpack expOutp)
    describe "basic page list combination" $ do
        it "unordered HTML list is generated" $ do
            let expOutp = T.unpack $ pagesToHTMLList tmpPages

            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            checkTheme "examples/page_list" expOutp
  where
    tmpPage1 = TextPage "Test page 1" "test-page1" "" True Nothing
    tmpPage2 = TextPage "Test page 2" "test-page2" "" True Nothing
    tmpPages = [tmpPage1, tmpPage2]
    -- | Helper for checking the output running the given theme in `themeDir`
    checkTheme themeDir expOutp = do
        yesod        <- getTestYesod
        outputBuffer <- liftIO $ newIORef ""

        let themeDir'     = "test/static/lua/" </> themeDir
            urlRenderer _ = "nop"
            currPlink     = (textPageSlug tmpPage1)
            lextra        = LuaExtra themeDir' currPlink (runDBIO yesod)
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
