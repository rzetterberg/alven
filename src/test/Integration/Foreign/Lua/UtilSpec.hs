{-# OPTIONS_GHC -fno-warn-orphans #-}

module Integration.Foreign.Lua.UtilSpec (spec) where

import qualified System.Process as P
import           TestImport hiding (assertEqual)
import           Test.HUnit (assertEqual, assertFailure)

import           Foreign.Lua.Util (getSourcePaths, findFilesAPICalls)

-------------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "getSourcePaths" $ do
        it "empty dir" $ do
            void $ P.system "rm -r tmp; mkdir tmp"

            src <- getSourcePaths "tmp"

            assertEqual "empty dir yields no results" [] src
        it "populated dir" $ do
            void $ P.system "rm -r tmp; mkdir tmp"
            void $ P.system "touch tmp/main.lua"
            void $ P.system "touch tmp/access.lua"

            src <- getSourcePaths "tmp"

            assertEqual "dir with 2 files" 2 (length src)
    describe "findFilesAPICalls" $ do
        it "1 file without API calls" $ do
            void $ P.system "rm -r tmp; mkdir tmp"
            void $ P.system "echo 'hehe' > tmp/main.lua"

            calls <- findFilesAPICalls ["tmp/main.lua"]

            case calls of
                ((_, occurs):_)
                    -> assertEqual "has no calls" 0 (length occurs)
                _
                    -> assertFailure "input one file, got none"
        it "1 file with 2 API calls" $ do
            void $ P.system "rm -r tmp; mkdir tmp"
            void $ P.system "echo 'alven.output()' > tmp/main.lua"
            void $ P.system "echo 'adwdqw qowdk wodqk' >> tmp/main.lua"
            void $ P.system "echo 'alven.get_pages()' >> tmp/main.lua"
            void $ P.system "echo 'adwdqw qowdk wodqk' >> tmp/main.lua"

            calls <- findFilesAPICalls ["tmp/main.lua"]

            case calls of
                ((_, occurs):_)
                    -> assertEqual "has 2 calls" 2 (length occurs)
                _
                    -> assertFailure "input one file, got none"
