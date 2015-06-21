module Layout.Public where

import           Import
import           Text.Lucius (luciusFile)

-------------------------------------------------------------------------------

singleLarge :: Widget -> Handler Html
singleLarge content = do
    userM <- maybeAuthId

    buildLayout $ do 
        toWidgetHead $(luciusFile "templates/layout/public/global.lucius")

        $(widgetFile "layout/public/single_large")

sidebar :: Widget -> Handler Html
sidebar content = do
    userM <- maybeAuthId

    buildLayout $ do 
        toWidgetHead $(luciusFile "templates/layout/public/global.lucius")

        $(widgetFile "layout/public/sidebar")
