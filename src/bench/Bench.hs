module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Layout.UtilBench as UtilBench

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ UtilBench.benches
                   ]
