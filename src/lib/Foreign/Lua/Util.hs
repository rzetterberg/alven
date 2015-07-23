{-|
Provides utility functionality related to Lua themes and API
-}
module Foreign.Lua.Util where

import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Filesystem.Path.CurrentOS (encodeString)
import           Import hiding (fileName)
import qualified System.FilePath.Find as Find
import           System.FilePath.Find ((~~?), always, fileName)

import           Foreign.Lua.Types

--------------------------------------------------------------------------------

{-|
Retrieves a list of paths to all Lua files in the given directory recursevely.
-}
getSourcePaths :: FilePath -> IO [FilePath]
getSourcePaths searchDir = do
    paths <- Find.find always (fileName ~~? "*.lua") (encodeString searchDir)

    mapM (return . fromString) paths

{-|
Generates a list of Lua source files where each source file has a list of
calls to the `alven` module.

Uses 'findAPICalls' on each file for extraction of call occurances.
-}
findFilesAPICalls :: [FilePath] -> IO [(FilePath, [Text])]
findFilesAPICalls = mapM processFile 
  where
    processFile :: FilePath -> IO (FilePath, [Text])
    processFile fp = do
        contents :: Text <- readFile fp 

        return (fp, findAPICalls contents)

{-|
Finds each place where the Lua API is used inside the given text blob. Returns a
list of each function from the API found.

See 'apiCallParser' for the parser definition.
-}
findAPICalls :: Text -> [Text]
findAPICalls = go . (parse apiCallParser)
  where
    go (Done rest result) = result : go (parse apiCallParser rest)
    go _                  = []

{-|
The parser used to find of API calls, matches against all data before the match
which makes it suitable to use multiple times to find multiple occurances in a
buffer.
-}
apiCallParser :: Parser Text
apiCallParser = do
        void $ manyTill anyChar (string "alven.")
        fname <- manyTill anyChar (char '(')
        void $ manyTill anyChar (char ')')

        return (pack fname)

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

-- | Tries to find an export by the given literal name
getExportByName :: String -> [LuaAPIExport] -> Maybe LuaAPIExport
getExportByName wantedName es = case (filter byName es) of
    (e:_) -> Just e
    _     -> Nothing
  where
    byName e = (getExportName e) == wantedName
