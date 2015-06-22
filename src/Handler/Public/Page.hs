module Handler.Public.Page where

import           Control.Concurrent (forkFinally)
import qualified Data.Text as T
import           Foreign.C.Types (CInt)
import           Import
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)

data ThemeResult
    = ThemeSuccess String
    | ThemeFailure String
    | ThemeRunning
      deriving (Show)

-------------------------------------------------------------------------------

getPageViewR :: Text -> Handler Html
getPageViewR permalink = do
    (Entity _ page) <- runDB $ getBy404 (UniquePageLink permalink)
    userM           <- maybeAuthId

    let isNotPublic   = not (textPagePublic page) 
        isNotLoggedIn = not (isJust userM)

    when (isNotPublic && isNotLoggedIn) notFound

    result <- liftIO $ do
        tres <- newEmptyMVar

        void $ forkFinally (runThemeScript page tres) (checkResult tres)

        readMVar tres

    return $ case result of
        ThemeSuccess outp -> toHtml outp
        ThemeFailure errm -> error errm
        ThemeRunning      -> error "Thread error"

  where
    checkResult tres (Left e) = putMVar tres $ ThemeFailure (show e)
    checkResult _ (Right _)   = return ()

--------------------------------------------------------------------------------
-- * Lua functionality

-- | Runs a theme script and puts the result in the given result MVar. Should be
--   run as a separate thread since the working directory is changed.
runThemeScript :: TextPage -> MVar ThemeResult -> IO ()
runThemeScript page result = do
    outputRef <- newIORef ""
    lstate    <- Lua.newstate

    Lua.openlibs lstate
    
    Lua.registerrawhsfunction lstate "print"
        (collectPrint outputRef)
    Lua.registerrawhsfunction lstate "get_current_page"
        (getCurrentPage page)

    Lua.loadfile lstate "output.lua"
        >>= runScript lstate outputRef
  where
    runScript lstate outputRef loadResult
        | loadResult == 0 = Lua.pcall lstate 0 0 0
                            >>= handleResult lstate outputRef
        | otherwise = do
           Lua.close lstate
           putMVar result $ ThemeFailure "Could not load theme file"
    handleResult lstate outputRef runResult
        | runResult == 0 = do
            Lua.close lstate
            output <- readIORef outputRef
            putMVar result $ ThemeSuccess output
        | otherwise = do
            errorMessage <- Lua.tostring lstate 1
            Lua.pop lstate 1
            Lua.close lstate
            putMVar result $ ThemeFailure errorMessage

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
