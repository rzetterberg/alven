module Foreign.Lua.Types where

import           Import 

-------------------------------------------------------------------------------

data LuaExtra = LuaExtra
    { permaLink    :: Text
    , dbRunner     :: IORunner
    , outputBuffer :: IORef String
    , urlRenderer  :: (Route App -> Text)
    }
