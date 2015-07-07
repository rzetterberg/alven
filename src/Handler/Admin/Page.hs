{-|
Provides the page management handlers for the admin interface. For the public
facing page related handlers see the "Handler.Public.Page" module.

These handlers provides functionality to list, edit, create and remove pages.
-}
module Handler.Admin.Page where

import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Import
import           Text.Markdown
import           Yesod.Text.Markdown

import qualified Model.TextPage as TextPageM
import qualified Layout.Admin as Layout

-------------------------------------------------------------------------------

{-|
Wrapper for 'getPageListR' that just supplies pageNo 0
-}
getPageListFirstR :: Handler Html
getPageListFirstR = getPageListR 0

{-|
Lists pages by ID ascending order, does not provide pagination. Each page in the
list can be viewed, edited and removed (except for the page with ID 1).

Show 'itemsPerPage' pages.
-}
getPageListR :: Int -> Handler Html
getPageListR pageNo = do
    pagesLen <- runDB $ count ([] :: [Filter TextPage])

    let offset     = TextPageM.pageToOffset pageNo
        limit      = TextPageM.pageToLimit pageNo
        lastPageNo = TextPageM.lenToLastPage pagesLen

    pages <- runDB $ TextPageM.getPaginated offset limit

    Layout.singleLarge "page-list" $ do
        setTitleI MsgPages

        $(widgetFile "blocks/admin/page_list")

{-|
Provides a editing form for an existing page. This handle is used for displaying
the form after the user has pressed the edit button of a page in the list.

See 'postPageEditR' for the handler that takes care of form validation and
saving the page in the database.
-}
getPageEditR :: TextPageId -> Handler Html
getPageEditR pageId = do
    page            <- runDB $ get404 pageId
    (form, encType) <- generateFormPost $ pageEditForm page
    msgRender       <- getJSMessageRender

    Layout.singleLarge "page-edit" $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgEditPage
    formUrl   = PageEditR pageId

{-|
Provides handling of submission of the edit form. Will display the form
validation in one response without redirecting when something failed to
validate.

If the page was successfully saved the user is redirected back to the
page list.
-}
postPageEditR :: TextPageId -> Handler Html
postPageEditR pageId = do
    page                      <- runDB $ get404 pageId
    ((result, form), encType) <- runFormPost $ pageEditForm page
    msgRender                 <- getJSMessageRender

    case result of
        FormSuccess changes -> save changes
        _                   -> return ()

    Layout.singleLarge "page-edit" $ do
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
        redirect PageListFirstR

{-|
Provides a create form for a new page. This handle is used for displaying
the form after the user has pressed the "create" button on the page list.

See 'postPageCreateR' for the handler that takes care of form validation and
saving the page in the database.
-}
getPageCreateR :: Handler Html
getPageCreateR = do
    (form, encType) <- generateFormPost pageCreateForm 
    msgRender       <- getJSMessageRender

    Layout.singleLarge "page-create" $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgCreatePage
    formUrl   = PageCreateR

{-|
Provides handling of submission of the create form. Will display the form
validation in one response without redirecting when something failed to
validate.

If the page was successfully saved the user is redirected back to the
page list.

NOTE: checks page slug collisions by querying the database by the given
page slug of the new page before saving. Race conditions can occur
between the select and the insert. If that happens, a duplicate key SqlError
will be thrown.
-}
postPageCreateR :: Handler Html
postPageCreateR = do
    ((result, form), encType) <- runFormPost pageCreateForm
    msgRender                 <- getJSMessageRender

    case result of
        FormSuccess new -> save new
        _               -> return ()

    Layout.singleLarge "page-create" $ do
        setTitleI pageTitle

        addScript $ StaticR js_marked_js
        addScript $ StaticR js_autosize_min_js

        $(widgetFile "blocks/admin/page_form")
  where
    pageTitle = MsgCreatePage
    formUrl   = PageCreateR
    save (PageEdit name plink body public) = do
        pageM <- runDB $ getBy (UniquePageLink plink)

        case pageM of
            Just _  -> do
                setAlertI Danger (MsgPageSlugExists plink)
                return ()
            Nothing -> do
                _ <- runDB $ insert $ TextPage name plink body public Nothing
        
                setAlertI Success MsgPageCreated
                redirect PageListFirstR

{-|
Provides a confirm page where the user has to accept the removal of the given
page. 

When the "yes" button is pressed the user is redirected to the 'postPageRemoveR'
handler for removal.
-}
postPageRemoveConfirmR :: TextPageId -> Handler Html
postPageRemoveConfirmR pageId = do
    page <- runDB $ get404 pageId

    Layout.singleLarge "page-remove-confirm" $ do
        setTitleI MsgRemovePage
        $(widgetFile "blocks/admin/page_remove_confirm")

{-|
Removes the page by the given id and redirects the user back to the page list
-}
postPageRemoveR :: TextPageId -> Handler Html
postPageRemoveR pageId = do
    if pageId == firstPageId
        then setAlertI Danger MsgCantRemoveFirstPage
        else runDB (delete pageId) >> setAlertI Success MsgPageRemoved

    redirect PageListFirstR

------------------------------------------------------------------------
-- * Forms

{-|
Partial data of a 'TextPage' to be used in forms
-}
data PageEdit = PageEdit
    { pageEditName      :: Text
    , pageEditPermalink :: Text
    , pageEditBody      :: Markdown
    , pageEditPublic    :: Bool
    } deriving (Show)

{-|
Provides a edit form from the given 'TextPage'.

Is a wrapper for the 'pageForm' function.
-}
pageEditForm :: TextPage -> Form PageEdit
pageEditForm (TextPage name plink body public _)
    = pageForm (Just name) (Just plink) (Just body) (Just public)

{-|
Provides a create form for a new page.

Is a wrapper for the 'pageForm' function.
-}
pageCreateForm :: Form PageEdit
pageCreateForm
    = pageForm Nothing Nothing Nothing Nothing

{-|
Provides a form for editable data of a 'TextPage'. Can be used for new pages
and existing pages.

Use wrappers 'pageEditForm' and 'pageCreateForm' for easier usage.
-}
pageForm :: Maybe Text     -- ^ Name
         -> Maybe Text     -- ^ Permalink
         -> Maybe Markdown -- ^ Body
         -> Maybe Bool     -- ^ Is the page public?
         -> Form PageEdit
pageForm nameM linkM bodyM publicM = Layout.renderForm $ PageEdit
    <$> areq textField (bfs MsgName) nameM
    <*> areq textField (bfs MsgPermalink) linkM
    <*> areq markdownField markdownSettings bodyM
    <*> areq Layout.bs3BoolField (bfs MsgIsPublic) publicM
    <*  bootstrapSubmit (BootstrapSubmit MsgSave "btn-success" [])
  where
    -- | Adds markdown-preview class to markdown field so that it can be
    --   progressively enhanced with javascript.
    markdownSettings = FieldSettings (SomeMessage MsgBody) Nothing Nothing Nothing
                       [("class", "form-control markdown-preview")]

------------------------------------------------------------------------
-- * Utilities

{-|
Provides functionality to render translated messages inside Julius code.
-}
getJSMessageRender :: (MonadHandler m, RenderMessage (HandlerSite m) message)
                   => m (message -> Value)
getJSMessageRender = getMessageRender >>= \r -> return (toJSON . r)

------------------------------------------------------------------------
-- * Constants

{-|
The id of the first page in the database.
-}
firstPageId :: TextPageId
firstPageId = toSqlKey 1
