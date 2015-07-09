{-|
Provides functionality to setup the Lua environment and run Lua themes.

To understand how a Lua theme is run you need to know:

* How resources are passed into the runner
* How information is passed between Lua and Haskell
* What a theme should consist of

This module provides a single function 'runThemeScript' that sets up the Lua
environment and executes the theme in the theme dir 

API functionality in this module refers to the functions that can be called in
Lua that executes Haskell code.

Some API functions needs to access the database and/or the output buffer
therefor those resources are kept in a central data type called 'LuaExtra'.
(See "Foreign.Lua.Types" for information about this data type contains).

When a Lua theme is executed it can access the Lua module __alven__ to use
all the API functions. All functions except for one are used to retrieve data
from the system (such as list of pages, specific pages, URLs, etc.). The
exception is the __output__ function that is used to append the given data to
a buffer that becomes the HTTP body after the theme has finished executing.

Currently this buffer is simply an "IORef" to a "Text" value that is appended
each time __output__ is called. 
-}
module Foreign.Lua where

import           Import 
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)
import           Filesystem.Path.CurrentOS (encodeString)

import qualified Foreign.Lua.API as API
import           Foreign.Lua.Types (LuaExtra(..), LuaAPIExport(..))

-------------------------------------------------------------------------------

{-|
Runs a theme script and returns the resulting output buffer or an error message
that consists of a Lua stack trace.

Adds right lua path to the given theme directory and registers all API functions
that are used to interact with alven from Lua. See "Foreign.Lua.API" for more
information.

Note: Expects that the given 'LuaExtra' contain valid resources. Does not check
whether the output buffer or db runner actually is setup correct.
-}
runThemeScript :: LuaExtra
               -> IO (Either String String)
runThemeScript lextra = do
    lstate <- Lua.newstate

    Lua.openlibs lstate

    addThemePaths lextra lstate
    registerAPIFunctions lstate lextra

    let mainScript = (runDir lextra) </> "main.lua"

    Lua.loadfile lstate (encodeString mainScript)
        >>= runScript lstate
  where
    runScript lstate loadResult
        | loadResult == 0 = do
            res <- Lua.pcall lstate 0 0 0
            handleResult lstate res
        | otherwise = returnError lstate
    handleResult lstate runResult
        | runResult == 0 = do
            Lua.close lstate
            readIORef (outputBuffer lextra) >>= return . Right
        | otherwise = returnError lstate
    returnError lstate = do
        errorMessage <- Lua.tostring lstate 1
        Lua.pop lstate 1
        Lua.close lstate
        return $ Left errorMessage

{-|
Adds search path to the directory where the theme is situated so that when a
Lua script uses require it will find the scripts in the same directory.

This is needed since the actual directory where the theme files are situated
is not the working directory of the application.
-}
addThemePaths :: LuaExtra
              -> LuaState
              -> IO ()
addThemePaths lextra lstate = do
    Lua.getglobal lstate "package"
    Lua.getfield lstate (-1) "path"

    currPath <- Lua.tostring lstate (-1)

    let tdir    = encodeString (runDir lextra)
        newPath =  currPath ++ ";"
                ++ "./" ++ tdir ++ "/?.lua;"
                ++ "./" ++ tdir ++ "/?/?.lua"

    Lua.pop lstate 1
    Lua.pushstring lstate newPath

    Lua.setfield lstate (-2) "path"
    Lua.pop lstate 1

{-|
Register all API functions in the current state as the module "alven".

See "API.exportedLuaFunctions" for the list of functions exported and the names
they are exported as.

For example a Haskell function `collectPrint` exported as `print`,
will be accessable in Lua using `alven:print("Hello, testing output")`.
-}
registerAPIFunctions :: LuaState
                     -> LuaExtra
                     -> IO ()
registerAPIFunctions lstate lextra = do
    let exports = API.exports lextra

    Lua.createtable lstate 0 (length exports)
    
    forM_ exports add

    Lua.setglobal lstate "alven"
  where
    add e@Exists{} = do
        Lua.pushrawhsfunction lstate (existsFunction e)
        Lua.setfield lstate (-2) (existsName e)
    add _          = return ()
