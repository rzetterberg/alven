{-|
Provides utility functionality related to Lua themes and API
-}
module Foreign.Lua.Util where

import           Data.Attoparsec.Text
import           Filesystem.Path.CurrentOS (encodeString)
import           Import hiding (fileName)
import qualified System.FilePath.Find as Find
import           System.FilePath.Find ((~~?), always, fileName)

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
