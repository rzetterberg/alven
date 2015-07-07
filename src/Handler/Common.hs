{-|
Provides common handlers for favicon, robot.txt, etc.
-}
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

{-|
Provides the current favicon that has been embedded in the binary.
-}
getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

{-|
Provides the current robot.txt that has been embedded in the binary.
-}
getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
