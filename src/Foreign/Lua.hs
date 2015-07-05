module Foreign.Lua where

import           Import 
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)
import           Filesystem.Path.CurrentOS (encodeString)

import qualified Foreign.Lua.API as API
import           Foreign.Lua.Types (LuaExtra(..))

-------------------------------------------------------------------------------

{-|
Runs a theme script and returns the resulting output buffer or an error message
that consists of a Lua stack trace.
-}
runThemeScript :: LuaExtra
               -> IO (Either String String)
runThemeScript lextra = do
    lstate <- Lua.newstate

    Lua.openlibs lstate

    addThemePaths lstate
    registerAPIFunctions lstate lextra

    let mainScript = themeDir </> "main.lua"

    Lua.loadfile lstate (encodeString mainScript)
        >>= runScript lstate
  where
    runScript lstate loadResult
        | loadResult == 0 = do
            res <- Lua.pcall lstate 0 0 0
            handleResult lstate res
        | otherwise = do
            Lua.close lstate
            return $ Left "Could not load theme file"
    handleResult lstate runResult
        | runResult == 0 = do
            Lua.close lstate
            readIORef (outputBuffer lextra) >>= return . Right
        | otherwise = do
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
addThemePaths :: LuaState
              -> IO ()
addThemePaths lstate = do
    Lua.getglobal lstate "package"
    Lua.getfield lstate (-1) "path"

    currPath <- Lua.tostring lstate (-1)

    let tdir    = encodeString themeDir
        newPath =  currPath ++ ";"
                ++ "./" ++ tdir ++ "/?.lua;"
                ++ "./" ++ tdir ++ "/?/?.lua"

    Lua.pop lstate 1
    Lua.pushstring lstate newPath

    Lua.setfield lstate (-2) "path"
    Lua.pop lstate 1

{-|
Register all API functions in the current state as the module "kael".

See "API.exportedLuaFunctions" for the list of functions exported and the names
they are exported as.

For example the Haskell function `collectPrint` will be exported as `print`,
which means you access it in Lua using `kael:print("Hello, testing output")`.
-}
registerAPIFunctions :: LuaState
                     -> LuaExtra
                     -> IO ()
registerAPIFunctions lstate lextra = do
    Lua.createtable lstate 0 (length funcs)
    
    forM_ funcs $ \(name, f) -> do
        Lua.pushrawhsfunction lstate f
        Lua.setfield lstate (-2) name

    Lua.setglobal lstate "kael"
  where
    funcs = API.funcTable lextra
