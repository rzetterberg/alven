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
data LuaAPIExport
    = Exists
      { existsName     :: String
      , existsVersion  :: (Int, Int)
      , existsFunction :: LuaAPIF
      }
    | Renamed
      { renamedName    :: String
      , renamedNewName :: String
      , renamedVersion :: (Int, Int)
      }
    | Removed
      { removedName    :: String
      , removedVersion :: (Int, Int)
      }

instance Eq LuaAPIExport where
    (==) Exists{ existsName = aname} Exists{ existsName = bname} = aname == bname
    (==) Renamed{ renamedName = aname} Renamed{ renamedName = bname} = aname == bname
    (==) Removed{ removedName = aname} Removed{ removedName = bname} = aname == bname
    (==) _ _ = False
    (/=) a b = not (a == b)

instance Show LuaAPIExport where
    show Exists{ existsName = n } = "Exists: " ++ n
    show Renamed{ renamedName = n } = "Renamed: " ++ n
    show Removed{ removedName = n } = "Removed: " ++ n

{-|
The data type that carries all the resources (database access, output buffer,
etc.) used in the Lua environment and the API functionality.
-}
data LuaExtra = LuaExtra
    { runDir       :: FilePath            -- ^ Dir where the theme is situated
    , slug         :: Text                -- ^ Slug of the current page
    , dbRunner     :: DBRunnerIO          -- ^ Db runner for the IO monad
    , outputBuffer :: IORef String        -- ^ All output from the Lua theme
    , urlRenderer  :: (Route App -> Text) -- ^ Renders URLs using current appRoot
    }
