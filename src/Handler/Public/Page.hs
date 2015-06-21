module Handler.Public.Page where

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
       then go
       else notFound
  where
    go = do
        res <- liftIO $ runPageTheme

        return $ case res of
            Nothing -> toHtml ("Error in Lua!" :: String)
            Just d  -> toHtml d

runPageTheme :: IO (Maybe String)
runPageTheme = do
    outputRef <- newIORef ""
    lstate    <- Lua.newstate

    Lua.openlibs lstate
    Lua.registerrawhsfunction lstate "print"
        (print_to_core outputRef)

    res <- Lua.loadfile lstate "test/lua/output.lua" >>= go lstate

    Lua.close lstate
       
    if res /= 0
       then return Nothing
       else readIORef outputRef >>= return . Just
  where
    go lstate 0 = Lua.pcall lstate 0 Lua.multret 0
    go _ _      = return 1

-- | Used to redefine Luas global print function to save output into a buffer
-- instead, so that the data can be accessed later via an `IORef`.
print_to_core :: IORef String -> LuaState -> IO CInt
print_to_core outputRef lstate = do
    luaData <- Lua.tostring lstate 1

    modifyIORef' outputRef (++ luaData)

    return 0
