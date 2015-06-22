module Handler.Public.Page where

import qualified Data.Text as T
import           Foreign.C.Types (CInt)
import           Import
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)

-------------------------------------------------------------------------------

getPageViewR :: Text -> Handler Html
getPageViewR permalink = do
    (Entity _ page) <- runDB $ getBy404 (UniquePageLink permalink)
    userM           <- maybeAuthId

    if (textPagePublic page) || (isJust userM)
       then go page
       else notFound
  where
    go page = do
        res <- liftIO $ runPageTheme page

        return $ case res of
            Left errMsg -> error errMsg
            Right outp  -> toHtml outp

runPageTheme :: TextPage -> IO (Either String String)
runPageTheme page = do
    outputRef <- newIORef ""
    lstate    <- Lua.newstate

    Lua.openlibs lstate
    Lua.registerrawhsfunction lstate "print"
        (collectPrint outputRef)
    Lua.registerrawhsfunction lstate "get_current_page"
        (getCurrentPage page)

    loadRes <- Lua.loadfile lstate "test/lua/output.lua" 

    when (loadRes /= 0) $ Lua.close lstate >> error "Could not load file"

    callRes <- Lua.pcall lstate 0 0 0

    ret <- if callRes /= 0
       then extractErrorMessage lstate
       else readIORef outputRef >>= return . Right

    Lua.close lstate

    return ret
  where
    extractErrorMessage lstate = do
        errMsg <- Lua.tostring lstate 1

        Lua.pop lstate 1

        return $ Left errMsg

--------------------------------------------------------------------------------
-- * Lua functions

-- | Used to redefine Luas global print function to save output into a buffer
-- instead, so that the data can be accessed later via an `IORef`.
collectPrint :: IORef String -> LuaState -> IO CInt
collectPrint outputRef lstate = do
    luaData <- Lua.tostring lstate 1

    modifyIORef' outputRef (++ luaData)

    return 0

getCurrentPage :: TextPage -> LuaState -> IO CInt
getCurrentPage TextPage{..} lstate = do
    Lua.createtable lstate 0 3

    Lua.pushstring lstate (T.unpack textPageName)
    Lua.setfield lstate (-2) "name"

    Lua.pushstring lstate (T.unpack textPagePermalink)
    Lua.setfield lstate (-2) "permalink"

    Lua.pushstring lstate "Not implemented yet"
    Lua.setfield lstate (-2) "body"

    return 1
