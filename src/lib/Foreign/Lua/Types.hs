{-|
Provides all data types used in the "Foreign.Lua" module and sub-modules.
-}
module Foreign.Lua.Types where

import qualified Data.Text as T
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

Functions that are 
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

-- | Checks if the given export is an existing one
exportExists :: LuaAPIExport -> Bool
exportExists Exists{} = True
exportExists _        = False

-- | Checks if the given export is a renamed one
exportRenamed :: LuaAPIExport -> Bool
exportRenamed Renamed{} = True
exportRenamed _         = False

-- | Checks if the given export is an removed one
exportRemoved :: LuaAPIExport -> Bool
exportRemoved Removed{} = True
exportRemoved _         = False

-- | Helper for getting the name of an export regardless of type
getExportName :: LuaAPIExport -> String
getExportName Exists{ existsName = n }   = n
getExportName Renamed{ renamedName = n } = n
getExportName Removed{ removedName = n } = n

-- | Helper for getting the version of an export regardless of type
getExportVersion :: LuaAPIExport -> (Int, Int)
getExportVersion Exists{ existsVersion = v }   = v
getExportVersion Renamed{ renamedVersion = v } = v
getExportVersion Removed{ removedVersion = v } = v

-- | Helper for getting the version as a literal of an export regardless of type
getExportVersionLit :: LuaAPIExport -> Text
getExportVersionLit e = let (major, minor) = getExportVersion e
                        in T.pack $ show major ++ "." ++ show minor

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
