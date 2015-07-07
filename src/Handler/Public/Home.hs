{-|
Provides the handlers for the home page routes.
-}
module Handler.Public.Home where

import           Import
import           Database.Persist.Sql (toSqlKey)

import qualified Layout.Public as Layout

-------------------------------------------------------------------------------

{-|
Shows the first page in the database by using id 1.

NOTE: this should be changed to call the page view handler in
"Handler.Public.Page".
-}
getHomeR :: Handler Html
getHomeR = do
    page  <- runDB $ get404 (toSqlKey 1)

    Layout.sidebar $ do
        setTitle $ toHtml (textPageName page)
        $(widgetFile "blocks/public/page_view")
