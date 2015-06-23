module Lua.Run where

import           Import 
import qualified Scripting.Lua as Lua
import           Scripting.Lua (LuaState)
import qualified Lua.API as API
import           Filesystem.Path.CurrentOS (encodeString)

-------------------------------------------------------------------------------

{-|
Runs a theme script and returns the resulting output buffer or an error message
that consists of a Lua stack trace.
-}
runThemeScript :: Text
               -> IORunner
               -> IO (Either String String)
runThemeScript permalink dbRunner = do
    outputRef <- newIORef ""
    lstate    <- Lua.newstate

    Lua.openlibs lstate

    addThemePaths lstate
    registerAPIFunctions lstate permalink dbRunner outputRef

    let mainScript = themeDir </> "main.lua"

    Lua.loadfile lstate (encodeString mainScript)
        >>= runScript lstate outputRef
  where
    runScript lstate outputRef loadResult
        | loadResult == 0 =   Lua.pcall lstate 0 0 0
                          >>= handleResult lstate outputRef
        | otherwise = do
           Lua.close lstate
           return $ Left "Could not load theme file"
    handleResult lstate outputRef runResult
        | runResult == 0 = do
            Lua.close lstate
            readIORef outputRef >>= return . Right
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

    print newPath

    Lua.pop lstate 1
    Lua.pushstring lstate newPath

    Lua.setfield lstate (-2) "path"
    Lua.pop lstate 1

{-|
Register all API functions in the current state see "Lua.API" for each
function registered.
-}
registerAPIFunctions :: LuaState
                     -> Text
                     -> IORunner
                     -> IORef String
                     -> IO ()
registerAPIFunctions lstate permalink dbRunner outputRef
    = forM_ funcs $ \(n, f) -> Lua.registerrawhsfunction lstate n f
  where
    funcs = [ ("print"           , API.collectPrint outputRef)
            , ("get_current_page", API.getCurrentPage dbRunner permalink)
            , ("read_theme_file" , API.readThemeFile)
            ]
