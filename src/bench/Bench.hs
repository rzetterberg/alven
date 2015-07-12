module Main where

import           Criterion.Main (defaultMain)
import           Prelude

import qualified Layout.UtilBench as LayoutUtilB
import qualified Foreign.Lua.UtilBench as LuaUtilB

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ LayoutUtilB.benches
                   , LuaUtilB.benches
                   ]
