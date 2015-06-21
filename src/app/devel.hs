{-# LANGUAGE PackageImports #-}

import           "kael" Application (develMain)
import           Control.Monad (void)
import           Prelude 
import           System.Process (runCommand)

main :: IO ()
main = do
    void $ runCommand "notify-send \"Yesod devel application launched\""
    develMain
