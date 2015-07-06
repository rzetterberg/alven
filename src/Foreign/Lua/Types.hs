{-|
Provides all data types used in the "Foreign.Lua" module and sub-modules.
-}
module Foreign.Lua.Types where

import           Import 

-------------------------------------------------------------------------------

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
