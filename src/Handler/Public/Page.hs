module Handler.Public.Page where

import           Import 
import qualified Lua.Run as Lua

-------------------------------------------------------------------------------

getPageViewR :: Text -> Handler Html
getPageViewR permalink = do
    master <- getYesod
    result <- liftIO $ Lua.runThemeScript permalink (runIO master)

    return $ case result of
        Left errm  -> error errm
        Right outp -> preEscapedToMarkup outp
