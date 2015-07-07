{-|
Provides the user management handlers for the admin interface. 

These handlers provides functionality to list, edit, create and remove users.
-}
module Handler.Admin.User where

import           Database.Persist.Sql (toSqlKey, fromSqlKey)
import           Import
import qualified Yesod.Auth.Email as Auth

import qualified Layout.Admin as Layout
import qualified Model.User as UserM

-------------------------------------------------------------------------------

{-|
Wrapper for 'getUserListR' that just supplies pageNo 0
-}
getUserListFirstR :: Handler Html
getUserListFirstR = getUserListR 0

{-|
Lists users by ID ascending order. Each user in the
list can be viewed, edited and removed (except for the page with ID 1).

Shows 'Model.itemsPerPage' users per page.
-}
getUserListR :: Int -> Handler Html
getUserListR pageNo = do
    (Pagination{..}, users) <- runDB $ UserM.getPaginated pageNo

    Layout.singleLarge "user-list" $ do
        setTitleI MsgUsers

        $(widgetFile "blocks/admin/user_list")

{-|
Provides an overview of the given user by id. Displays information such as if
the user is an admin, if it is verified, etc.
-}
getUserViewR :: UserId -> Handler Html
getUserViewR userId = do
    user <- runDB $ get404 userId

    Layout.singleLarge "user-view" $ do
        setTitleI MsgUser
        $(widgetFile "blocks/admin/user_view")

{-|
Provides a editing form for an existing user. This handle is used for displaying
the form after the user has pressed the edit button of a user in the list.

See 'postUserEditR' for the handler that takes care of form validation and
saving the user in the database.
-}
getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
    user            <- runDB $ get404 userId
    (form, encType) <- generateFormPost $ userEditForm user

    Layout.singleLarge "user-edit" $ do
        setTitleI MsgEditUser
        $(widgetFile "blocks/admin/user_edit")

{-|
Provides handling of submission of the edit form. Will display the form
validation in one response without redirecting when something failed to
validate.

If the user (the data) was successfully saved the user (the visitor) is
redirected back to the page list.
-}
postUserEditR :: UserId -> Handler Html
postUserEditR userId = do
    user                      <- runDB $ get404 userId
    ((result, form), encType) <- runFormPost $ userEditForm user

    case result of
        FormSuccess changes -> save changes
        _                   -> return ()

    Layout.singleLarge "user-edit" $ do
        setTitleI MsgEditUser
        $(widgetFile "blocks/admin/user_edit")
  where
    save (UserEdit _ passM admin) = do
        passChange <- case passM of
            Nothing -> return []
            Just p  -> if null p
                then return []
                else do
                    spass <- liftIO $ Auth.saltPass p
                    return [UserPassword =. (Just spass)]

        runDB $ update userId $
            [UserAdmin =. admin] ++ passChange

        setAlertI Success MsgUserUpdated
        redirect UserListFirstR

{-|
Provides a create form for a new user. This handle is used for displaying
the form after the user has pressed the "create" button on the page list.

See 'postUserCreateR' for the handler that takes care of form validation and
saving the user in the database.
-}
getUserCreateR :: Handler Html
getUserCreateR = do
    (form, encType) <- generateFormPost userCreateForm 

    Layout.singleLarge "user-create" $ do
        setTitleI MsgCreateUser
        $(widgetFile "blocks/admin/user_create")

{-|
Provides handling of submission of the create form. Will display the form
validation in one response without redirecting when something failed to
validate.

If the user (the data) was successfully saved the user (the visitor) is
redirected back to the page list.

NOTE: does not check unique email collisions, will throw an duplicate key
SqlError if a collision occured.
-}
postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, form), encType) <- runFormPost userCreateForm

    case result of
        FormSuccess u -> save u
        _             -> return ()

    Layout.singleLarge "user-create" $ do
        setTitleI MsgCreateUser
        $(widgetFile "blocks/admin/user_create")
  where
    save (UserCreate email admin) = do
        master <- getYesod
        vkey   <- liftIO $ Auth.randomKey master
        render <- getUrlRender
        newUID <- runDB $ insert $ User email Nothing (Just vkey) False admin

        let verUrl = render $ AuthR $ PluginR "email" ["verify", (toPathPiece newUID), vkey]

        Auth.sendVerifyEmail email vkey verUrl

        setAlertI Success MsgUserCreated
        redirect UserListFirstR

{-|
Provides a confirm page where the user has to accept the removal of the given
user. 

When the "yes" button is pressed the user is redirected to the 'postUserRemoveR'
handler for removal.
-}
postUserRemoveConfirmR :: UserId -> Handler Html
postUserRemoveConfirmR userId = do
    user <- runDB $ get404 userId

    Layout.singleLarge "user-remove-confirm" $ do
        setTitleI MsgRemoveUser
        $(widgetFile "blocks/admin/user_remove_confirm")

{-|
Removes the user by the given id and redirects the user back to the user list
-}
postUserRemoveR :: UserId -> Handler Html
postUserRemoveR userId
    = if userId == firstUserId
      then do
          setAlertI Danger MsgCantDeleteRoot
          redirect UserListFirstR
      else do
          runDB $ delete userId

          setAlertI Success MsgUserRemoved
          redirect UserListFirstR

------------------------------------------------------------------------
-- * Forms

{-|
Partial data of a 'User' to be used in forms when editing a user. 
-}
data UserEdit = UserEdit
    { userEditEmail    :: Text
    , userEditPassword :: (Maybe Text)
    , userEditAdmin    :: Bool
    } deriving Show

{-|
Partial data of a 'User' to be used in forms when creating a user, differs
from 'UserEdit' by not providing a password field since a link to the password
choose page is sent to the users email.
-}
data UserCreate = UserCreate
    { userCreateEmail :: Text
    , userCreateAdmin :: Bool
    } deriving Show

{-|
Provides a form for editable data of an existing 'User'. Can only be used when
editing an existing user, see 'userCreateForm' for creating new users.
-}
userEditForm :: User -> Form UserEdit
userEditForm user = Layout.renderForm $ UserEdit
    <$> areq Layout.bs3StaticTextField (bfs MsgEmail) emailM
    <*> aopt passwordConfirmField (bfs MsgNewPassword) Nothing
    <*> areq Layout.bs3BoolField (bfs MsgIsAdmin) adminM
    <*  bootstrapSubmit (BootstrapSubmit MsgSave "btn-success" [])
  where
    emailM = Just (userEmail user)
    adminM = Just (userAdmin user)

{-|
Provides a form for editable data of a new 'User'. Can only be used when
creating a new user.
-}
userCreateForm :: Form UserCreate
userCreateForm = Layout.renderForm $ UserCreate
    <$> areq emailField (bfs MsgEmail) Nothing
    <*> areq Layout.bs3BoolField (bfs MsgIsAdmin) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgSave "btn-success" [])

{-|
Provides a field where the password has to be filled in twice and be matching
to avoid users filling in an invalid password.
-}
passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b    -> return $ Right $ Just a
                | otherwise -> return $ Left (SomeMessage MsgPasswordsDidntMatch)
            [] -> return $ Right Nothing
            _  -> return $ Left (SomeMessage MsgPasswordTwice)
    , fieldView = \idAttr nameAttr otherAttrs _ _ -> [whamlet|
<p>
  <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=password placeholder=_{MsgPassword}>
<p>
  <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password placeholder=_{MsgPasswordAgain}>
|]
    , fieldEnctype = UrlEncoded
    }

------------------------------------------------------------------------
-- * Constants

{-|
The id of the first user in the database.
-}
firstUserId :: UserId
firstUserId = toSqlKey 1
