module Foreign.Lua.UtilBench (benches) where

import           Criterion.Types
import           Data.FileEmbed (embedFile)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text)
import           Prelude

import           Foreign.Lua.Util (findAPICalls)

--------------------------------------------------------------------------------

benches :: Benchmark
benches = bgroup "findAPICalls" $ map build
    [ ( "using a small theme buffer with multiple occurances"
      , findAPICalls
      , smallTheme
      )
    , ( "using a large theme buffer with multiple occurances"
      , findAPICalls
      , largeTheme
      )
    , ( "using large lorem ipsum buffer with no occurances"
      , findAPICalls
      , largeLorem
      )
    , ( "using large random buffer with no occurances"
      , findAPICalls
      , largeRandom
      )
    ]
  where
    build (label, f, args) = bench label $ nf f args

{-|
A small Lua theme that contains valid code with multiple occurances of alven
API calls.
-}
smallTheme :: Text
smallTheme = decodeUtf8 $(embedFile "bench/static/lua/small_theme.lua")

{-|
A large Lua theme that contains valid code with multiple occurances of alven
API calls.
-}
largeTheme :: Text
largeTheme = decodeUtf8 $(embedFile "bench/static/lua/large_theme.lua")

{-|
A large buffer with lorem ipsum text. THe lorem ipsum text has been copied
multiple times which makes the data repetitable.
-}
largeLorem :: Text
largeLorem = decodeUtf8 $(embedFile "bench/static/text/large_lorem.txt")

{-|
A large buffer with random data encoded as base64, should not contain any
repetitive patterns at all since the data was pulled from /dev/urandom.
-}
largeRandom :: Text
largeRandom = decodeUtf8 $(embedFile "bench/static/text/large_random.txt")
