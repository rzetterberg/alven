module Handler.Admin.Page where

import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Import
import           Text.Markdown
import           Yesod.Text.Markdown

import qualified Layout.Admin as Layout

-------------------------------------------------------------------------------

getPageListR :: Handler Html
getPageListR = do
    allPages :: [Entity TextPage] <- runDB $ selectList [] [Asc TextPageId]

    Layout.singleLarge $ do
        setTitleI MsgPages

        $(widgetFile "blocks/admin/page_list")

getPageEditR :: TextPageId -> Handler Html
getPageEditR pageId = do
    page            <- runDB $ get404 pageId
    (form, encType) <- generateFormPost $ pageEditForm page
    msgRender       <- getJSMessageRender

    Layout.singleLarge $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgEditPage
    formUrl   = PageEditR pageId

postPageEditR :: TextPageId -> Handler Html
postPageEditR pageId = do
    page                      <- runDB $ get404 pageId
    ((result, form), encType) <- runFormPost $ pageEditForm page
    msgRender                 <- getJSMessageRender

    case result of
        FormSuccess changes -> save changes
        _                   -> return ()

    Layout.singleLarge $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgEditPage
    formUrl   = PageEditR pageId
    save (PageEdit name plink body public) = do
        runDB $ update pageId [ TextPageName      =. name
                              , TextPagePermalink =. plink
                              , TextPageBody      =. body
                              , TextPagePublic    =. public
                              ]
        
        setAlertI Success MsgPageUpdated
        redirect PageListR

getPageCreateR :: Handler Html
getPageCreateR = do
    (form, encType) <- generateFormPost pageCreateForm 
    msgRender       <- getJSMessageRender

    Layout.singleLarge $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgCreatePage
    formUrl   = PageCreateR

postPageCreateR :: Handler Html
postPageCreateR = do
    ((result, form), encType) <- runFormPost pageCreateForm
    msgRender                 <- getJSMessageRender

    case result of
        FormSuccess new -> save new
        _               -> return ()

    Layout.singleLarge $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgCreatePage
    formUrl   = PageCreateR
    save (PageEdit name plink body public) = do
        _ <- runDB $ insert $ TextPage name plink body public Nothing
        
        setAlertI Success MsgPageCreated
        redirect PageListR

postPageRemoveConfirmR :: TextPageId -> Handler Html
postPageRemoveConfirmR pageId = do
    page <- runDB $ get404 pageId

    Layout.singleLarge $ do
        setTitleI MsgRemovePage
        $(widgetFile "blocks/admin/page_remove_confirm")

postPageRemoveR :: TextPageId -> Handler Html
postPageRemoveR pageId = do
    if pageId == firstPageId
        then setAlertI Danger MsgCantRemoveFirstPage
        else runDB (delete pageId) >> setAlertI Success MsgPageRemoved

    redirect PageListR

------------------------------------------------------------------------
-- * Forms

data PageEdit = PageEdit Text Text Markdown Bool

pageEditForm :: TextPage -> Form PageEdit
pageEditForm (TextPage name plink body public _)
    = pageForm (Just name) (Just plink) (Just body) (Just public)

pageCreateForm :: Form PageEdit
pageCreateForm
    = pageForm Nothing Nothing Nothing Nothing

pageForm :: Maybe Text -> Maybe Text -> Maybe Markdown -> Maybe Bool -> Form PageEdit
pageForm nameM linkM bodyM publicM = Layout.renderForm $ PageEdit
    <$> areq textField (bfs MsgName) nameM
    <*> areq textField (bfs MsgPermalink) linkM
    <*> areq markdownField markdownSettings bodyM
    <*> areq Layout.bs3BoolField (bfs MsgIsPublic) publicM
    <*  bootstrapSubmit (BootstrapSubmit MsgSave "btn-success" [])
  where
    -- | Adds markdown-preview class to markdown field so that it can be
    -- progressively enhanced with javascript.
    markdownSettings = FieldSettings (SomeMessage MsgBody) Nothing Nothing Nothing
                       [("class", "form-control markdown-preview")]

------------------------------------------------------------------------
-- * Utilities

getJSMessageRender :: (MonadHandler m, RenderMessage (HandlerSite m) message)
                   => m (message -> Value)
getJSMessageRender = getMessageRender >>= \r -> return (toJSON . r)

------------------------------------------------------------------------
-- * Constants

firstPageId :: TextPageId
firstPageId = toSqlKey 1
