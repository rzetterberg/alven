module Handler.Public.Page where

import           Import 
import           Lua.Run (runThemeScript)

-------------------------------------------------------------------------------

getPageViewR :: Text -> Handler Html
getPageViewR permalink = do
    master <- getYesod
    result <- liftIO $ runThemeScript permalink (runIO master)

    return $ case result of
        Left errm  -> error errm
        Right outp -> preEscapedToMarkup outp
