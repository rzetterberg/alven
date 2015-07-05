module Handler.Public.Page where

import           Import 
import qualified Foreign.Lua as Lua
import           Foreign.Lua.Types (LuaExtra(..))

-------------------------------------------------------------------------------

getPageViewR :: Text -> Handler Html
getPageViewR permalink = do
    yesod        <- getYesod
    outputBuffer <- newIORef ""
    urlRenderer  <- getUrlRender

    let lextra = LuaExtra permalink (runIO yesod) outputBuffer urlRenderer
    
    result <- liftIO $ Lua.runThemeScript lextra

    return $ case result of
        Left errm  -> error errm
        Right outp -> preEscapedToMarkup outp
