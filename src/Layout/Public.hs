{-|
Layout rendering functionality for different types of layouts used by public
accessible pages.

All layouts are Bootstrap based which means grid units refer to the relative
size of 1 to 12.
-}
module Layout.Public where

import           Import
import           Text.Lucius (luciusFile)

-------------------------------------------------------------------------------

{-|
1-column layout with content area of 12 grid units
-}
singleLarge :: Widget -> Handler Html
singleLarge content = do
    userM <- maybeAuthId

    buildLayout $ do 
        toWidgetHead $(luciusFile "templates/layout/public/global.lucius")

        $(widgetFile "layout/public/single_large")

{-|
2-column layout with content area and sidebar using a 8:4 ratio
-}
sidebar :: Widget -> Handler Html
sidebar content = do
    userM <- maybeAuthId

    buildLayout $ do 
        toWidgetHead $(luciusFile "templates/layout/public/global.lucius")

        $(widgetFile "layout/public/sidebar")
