module Foreign.Lua.Types where

import           Import 

-------------------------------------------------------------------------------

data LuaExtra = LuaExtra
    { runDir       :: FilePath
    , permaLink    :: Text
    , dbRunner     :: DBRunnerIO
    , outputBuffer :: IORef String
    , urlRenderer  :: (Route App -> Text)
    }
