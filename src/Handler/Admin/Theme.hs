{-|
Provides the functionality to facilitate the theme usage.
-}
module Handler.Admin.Theme where

import           Import

import qualified Layout.Admin as Layout
import           Foreign.Lua.API (getExports)
import           Foreign.Lua.Types 

-------------------------------------------------------------------------------

{-|
Provides a list of all API functions that can be used in Lua themes along with
their status. See 'LuaAPIExport' for more information.
-}
getThemeIndexR :: Handler Html
getThemeIndexR = do
    yesod        <- getYesod
    outputBuffer <- newIORef ""
    urlRenderer  <- getUrlRender

    let lextra = LuaExtra themeDir "" (runDBIO yesod)
                          outputBuffer urlRenderer
        existingExports = filter exportExists (getExports lextra)
    
    Layout.singleLarge "theme-index" $ do
        setTitleI MsgTheme

        $(widgetFile "blocks/admin/theme_index")
