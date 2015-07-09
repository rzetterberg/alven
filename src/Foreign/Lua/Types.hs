{-|
Provides all data types used in the "Foreign.Lua" module and sub-modules.
-}
module Foreign.Lua.Types where

import           Foreign.C.Types (CInt)
import           Import 
import           Scripting.Lua (LuaState)

-------------------------------------------------------------------------------

{-|
The alias for Haskell functions that are called from Lua
-}
type LuaAPIF = (LuaState -> IO CInt)

{-|
Represents a function that is exported to Lua. Contains the name it can be
accessed by in Lua, which version it was introduced and the actual Haskell
function that is exported.
-}
data LuaAPIExport = LuaAPIExport
    { luaFunctionName   :: String
    , versionIntroduced :: (Int, Int)
    , exportedFunction  :: LuaAPIF
    }

instance Eq LuaAPIExport where
    (==) a b = (luaFunctionName a) == (luaFunctionName b)
    (/=) a b = not (a == b)

{-|
The data type that carries all the resources (database access, output buffer,
etc.) used in the Lua environment and the API functionality.
-}
data LuaExtra = LuaExtra
    { runDir       :: FilePath            -- ^ Dir where the theme is situated
    , permaLink    :: Text                -- ^ Permalink of the current page
    , dbRunner     :: DBRunnerIO          -- ^ Db runner for the IO monad
    , outputBuffer :: IORef String        -- ^ All output from the Lua theme
    , urlRenderer  :: (Route App -> Text) -- ^ Renders URLs using current appRoot
    }
