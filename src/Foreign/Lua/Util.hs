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
Generates a list lines where calls to the Lua module alven was found for each
file in the given list of file paths.

NOTE: Currently uses a very naive search function by just matching
against the string "alven.". This should later be changed to either
using a parser or a regexp.
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
-}
findAPICalls :: Text -> [Text]
findAPICalls = go . (parse apiCallParser)
  where
    go (Done rest result) = result : go (parse apiCallParser rest)
    go _                  = []

{-|
The parser used to find of API calls, matches against all data before the match
which makes it suitable to use multiple times.
-}
apiCallParser :: Parser Text
apiCallParser = do
        void $ manyTill anyChar (string "alven.")
        fname <- manyTill anyChar (char '(')
        void $ manyTill anyChar (char ')')

        return (pack fname)
