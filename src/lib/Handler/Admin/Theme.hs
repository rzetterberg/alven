{-|
Provides the functionality to facilitate the theme usage.
-}
module Handler.Admin.Theme where

import           Data.ByteString.Lazy (fromStrict)
import           Data.FileEmbed (embedFile)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Filesystem.Path.CurrentOS (encodeString)
import           Import hiding (decodeUtf8, fromStrict, fileName)
import           Text.Markdown (markdown)

import           Foreign.Lua.API (getExports)
import           Foreign.Lua.Types 
import           Foreign.Lua.Util (getSourcePaths, findFilesAPICalls) 
import qualified Layout.Admin as Layout

-------------------------------------------------------------------------------

{-|
Provides a page where you can choose to use theme related functionality.
-}
getThemeIndexR :: Handler Html
getThemeIndexR = do
    Layout.singleLarge "theme-index" $ do
        setTitleI MsgTheme

        $(widgetFile "blocks/admin/theme_index")

{-|
Provides a list of all API functions that can be used in Lua themes along with
their status. See 'LuaAPIExport' for more information.

Provides the documentation for each function via the theme_api_reference.md file
in the static/markdown directory.
-}
getThemeAPIReferenceR :: Handler Html
getThemeAPIReferenceR = do
    yesod        <- getYesod
    outputBuffer <- newIORef ""
    urlRenderer  <- getUrlRender

    let lextra  = LuaExtra themeDir "" (runDBIO yesod)
                           outputBuffer urlRenderer
        exports = getExports lextra
        apiRef  = $(embedFile "static/markdown/theme_api_reference.md")

    let referenceContent = markdown def $ decodeUtf8 (fromStrict apiRef)
    
    Layout.singleLarge "theme-api-reference" $ do
        setTitleI MsgAPIReference

        $(widgetFile "blocks/admin/theme_api_reference")

{-|
Checks if the current theme is compatible with the current version of the
project.
-}
getThemeCompabilityCheckR :: Handler Html
getThemeCompabilityCheckR = do
    sourcePaths      <- liftIO $ getSourcePaths themeDir
    processedSources <- liftIO $ findFilesAPICalls sourcePaths

    Layout.singleLarge "theme-compability-check" $ do
        setTitleI MsgCompabilityCheck

        $(widgetFile "blocks/admin/theme_compability_check")
