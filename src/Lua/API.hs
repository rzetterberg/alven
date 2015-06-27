module Lua.API where

import qualified Data.Text as T
import           Foreign.C.Types (CInt)
import           Import 
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)

--------------------------------------------------------------------------------

{-|
Appends a string to an output buffer. Is used to replace Lua's standard print
to collect all output into a buffer that can later be used in Haskell.

Lua prototype: void print(string)
-}
collectPrint :: IORef String
             -> LuaState
             -> IO CInt
collectPrint outputRef lstate = do
    luaData <- Lua.tostring lstate 1

    modifyIORef' outputRef (++ luaData)

    return 0

{-|
Retrieves the current page by permalink. Returns the page as a table or nil
if the page was not found in the database.

Lua prototype: table get_current_page(void)
-}
getCurrentPage :: IORunner -- ^ Database runner for usage in IO
               -> Text     -- ^ Page permalink
               -> LuaState -- ^ Current state
               -> IO CInt
getCurrentPage dbRunner permalink lstate = do
    pageM <- dbRunner $ getBy (UniquePageLink permalink)

    case pageM of
        Nothing           -> Lua.pushnil lstate
        Just (Entity _ p) -> textPageToLua lstate p

    return 1

{-|
Retrieves a list of all pages in the database. Can be used to render site
navigation.
-}
getPages :: IORunner -- ^ Database runner for usage in IO
         -> LuaState -- ^ Current state
         -> IO CInt
getPages dbRunner lstate = do
    pages <- dbRunner (selectList [] [])

    Lua.newtable lstate

    go pages 0

    return 1
  where
    go :: [Entity TextPage] -> Int -> IO ()
    go [] _              = return ()
    go ((Entity _ p):ps) n = do
        textPageToLua lstate p

        Lua.rawseti lstate (-2) (n + 1)

        go ps (n + 1)

{-|
Reads a file in the theme folder and returns it as a string, throws error if
read failed.

Lua prototype: string read_theme_file(string)
-}
readThemeFile :: LuaState
              -> IO CInt
readThemeFile lstate = do
    relPath <- Lua.tostring lstate 1

    let absPath = themeDir </> (fromString relPath)
    
    templateM <- try $ readFile absPath

    checkResult templateM
  where
    checkResult :: Either SomeException String -> IO CInt
    checkResult (Left e)  = do
        let errMsg = "load_template failed with:\n" ++ show e

        Lua.pushstring lstate errMsg
        return (-1)
    checkResult (Right t) = Lua.pushstring lstate t >> return 1

--------------------------------------------------------------------------------
-- * Data type marshalling

{-|
Converts the given `TextPage` into a Lua table with fields name, permalink
and body.
-}
textPageToLua :: LuaState -- ^ The current state
              -> TextPage -- ^ Text page to marshall
              -> IO ()
textPageToLua lstate TextPage{..} = do
    Lua.createtable lstate 0 3

    Lua.pushstring lstate (T.unpack textPageName)
    Lua.setfield lstate (-2) "name"

    Lua.pushstring lstate (T.unpack textPagePermalink)
    Lua.setfield lstate (-2) "permalink"

    let tbody = renderHtml $ toHtml textPageBody

    Lua.pushstring lstate tbody
    Lua.setfield lstate (-2) "body"
