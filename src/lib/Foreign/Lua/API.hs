{-|
Provies all the API functions that can be called from Lua to run Haskell code.

The function 'getExports' contains a list of all existing, renamed and removed
API functions. View the source of this module to see what Lua names they are
exported as.
-}
module Foreign.Lua.API where

import qualified Data.Text as T
import           Foreign.C.Types (CInt)
import           Import 
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)

import qualified Model.TextPage as TextPageM
import           Foreign.Lua.Types (LuaExtra(..), LuaAPIF, LuaAPIExport(..))

--------------------------------------------------------------------------------

{-|
All functions exported to Lua and the name they will be exported as.

See 'LuaAPIExport' for information about each export.

For exampe 'getThemeURL' will be exported as __get_theme_url__ and can be
accessed in Lua such as:

> alven.get_theme_url("main.css")
-}
getExports :: [LuaAPIExport]
getExports 
    = [ Exists "output"           (0, 1) output
      , Exists "get_theme_url"    (0, 1) getThemeURL
      , Exists "get_current_page" (0, 1) getCurrentPage
      , Exists "get_pages"        (0, 1) getPages
      , Exists "read_theme_file"  (0, 1) readThemeFile
      ]

--------------------------------------------------------------------------------
-- * Core

{-|
Appends a string to the output buffer. This buffer will become the HTTP body of
the current response.
-}
output :: LuaAPIF
output LuaExtra{..} lstate = do
    luaData <- Lua.tostring lstate 1

    modifyIORef' outputBuffer (++ luaData)

    return 0

{-|
Retrives the absolute URL to a file within the current theme.

Can be used to link to static content such as images, css, javascript.
-}
getThemeURL :: LuaAPIF
getThemeURL LuaExtra{..} lstate = do
    fname <- Lua.tostring lstate 1

    let pieces  = T.splitOn "/" (T.pack fname)
        route = StaticRoute ("theme" : pieces) []
        url   = urlRenderer $ StaticR route

    Lua.pushstring lstate (T.unpack url)

    return 1

--------------------------------------------------------------------------------
-- * Page retrieval

{-|
Retrieves the current page by slug. Returns the page as a table or nil
if the page was not found in the database.
-}
getCurrentPage :: LuaAPIF
getCurrentPage lextra@LuaExtra{..} lstate = do
    pageM <- dbRunner $ TextPageM.getCurrPublic slug

    case pageM of
        Nothing           -> Lua.pushnil lstate
        Just (Entity _ p) -> textPageToLua lextra lstate p

    return 1

{-|
Retrieves a list of all public pages in the database. Can be used to render site
navigation.
-}
getPages :: LuaAPIF
getPages lextra@LuaExtra{..} lstate = do
    pages <- dbRunner TextPageM.getPublic

    Lua.newtable lstate

    go pages 0

    return 1
  where
    go :: [Entity TextPage] -> Int -> IO ()
    go [] _              = return ()
    go ((Entity _ p):ps) n = do
        textPageToLua lextra lstate p

        Lua.rawseti lstate (-2) (n + 1)

        go ps (n + 1)

--------------------------------------------------------------------------------
-- * File access

{-|
Reads a file in the theme folder and returns it as a string, throws error if
read failed.
-}
readThemeFile :: LuaAPIF
readThemeFile lextra lstate = do
    relPath <- Lua.tostring lstate 1

    let absPath = (runDir lextra) </> (fromString relPath)
    
    templateM <- try $ readFile absPath

    checkResult templateM
  where
    checkResult :: Either SomeException String -> IO CInt
    checkResult (Left e)  = do
        let errMsg = "readThemeFile with:\n" ++ show e

        Lua.pushstring lstate errMsg
        return (-1)
    checkResult (Right t) = Lua.pushstring lstate t >> return 1

--------------------------------------------------------------------------------
-- * Data type marshalling

{-|
Converts the given `TextPage` into a Lua table with fields name, slug, body
and url.

Note: Url is not part of the original `TextPage`, but is added for convinience.
-}
textPageToLua :: LuaExtra
              -> LuaState
              -> TextPage
              -> IO ()
textPageToLua LuaExtra{..} lstate TextPage{..} = do
    Lua.createtable lstate 0 6

    Lua.pushstring lstate (T.unpack textPageName)
    Lua.setfield lstate (-2) "name"

    Lua.pushstring lstate (T.unpack textPageSlug)
    Lua.setfield lstate (-2) "slug"

    let tbody = renderHtml $ toHtml textPageBody

    Lua.pushstring lstate tbody
    Lua.setfield lstate (-2) "body"

    Lua.pushboolean lstate textPagePublic
    Lua.setfield lstate (-2) "is_public"

    let absPageURL = urlRenderer (PageViewR textPageSlug)

    Lua.pushstring lstate (T.unpack absPageURL)
    Lua.setfield lstate (-2) "url"

    Lua.pushboolean lstate (textPageSlug == slug)
    Lua.setfield lstate (-2) "is_active"
