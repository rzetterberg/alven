module Handler.Public.Home where

import           Import
import           Database.Persist.Sql (toSqlKey)

import qualified Layout.Public as Layout

-------------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
    page  <- runDB $ get404 (toSqlKey 1)

    Layout.sidebar $ do
        setTitle $ toHtml (textPageName page)
        $(widgetFile "blocks/public/page_view")
