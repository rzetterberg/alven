module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Layout.AdminBench as AdminBench

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ AdminBench.benches
                   ]
