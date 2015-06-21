module Handler.Public.Page where

import           Import

import qualified Layout.Public as Layout

-------------------------------------------------------------------------------

getPageViewR :: Text -> Handler Html
getPageViewR permalink = do
    (Entity _ page) <- runDB $ getBy404 (UniquePageLink permalink)
    userM           <- maybeAuthId

    if (textPagePublic page) ||  (isJust userM)
       then Layout.sidebar $ do
           setTitle $ toHtml (textPageName page)
           $(widgetFile "blocks/public/page_view")
       else notFound
