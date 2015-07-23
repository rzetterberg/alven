{-|
Provides the handlers for public page routes.
-}
module Handler.Public.Page where

import           Import 
import qualified Foreign.Lua as Lua
import           Foreign.Lua.Types (LuaExtra(..))

-------------------------------------------------------------------------------

{-|
Selects the first page in the database and runs 'getPageViewR' handler with the
page slug. If no page is in the database and empty string is used.
-}
getPageHomeR :: Handler Html
getPageHomeR = do
    page  <- runDB $ selectList [] [LimitTo 1]
    
    let pslug = case page of
                    []    -> ""  
                    (Entity _ p:_) -> textPageSlug p

    getPageViewR pslug

{-|
Loads and executes the current Lua theme for the page of the given
slug/permalink.

Errors that occured inside Lua will be displayed as a normal 500 error
page.

NOTE: showing errors should be disabled in production systems, see
<http://stackoverflow.com/q/20318666 this stackoverflow thread>
for more information.
-}
getPageViewR :: Text -> Handler Html
getPageViewR slug = do
    yesod        <- getYesod
    outputBuffer <- newIORef ""
    urlRenderer  <- getUrlRender

    let lextra = LuaExtra themeDir slug (runDBIO yesod)
                          outputBuffer urlRenderer
    
    result <- liftIO $ Lua.runThemeScript lextra

    return $ case result of
        Left errm  -> error errm
        Right outp -> preEscapedToMarkup outp
