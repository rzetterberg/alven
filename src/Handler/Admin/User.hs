module Handler.Admin.User where

import           Database.Persist.Sql (toSqlKey, fromSqlKey)
import           Import
import qualified Yesod.Auth.Email as Auth

import qualified Layout.Admin as Layout

-------------------------------------------------------------------------------

getUserListR :: Handler Html
getUserListR = do
    allUsers :: [Entity User] <- runDB $ selectList [] [Asc UserId]

    Layout.singleLarge "user-list" $ do
        setTitleI MsgUsers

        $(widgetFile "blocks/admin/user_list")

getUserViewR :: UserId -> Handler Html
getUserViewR userId = do
    user <- runDB $ get404 userId

    Layout.singleLarge "user-view" $ do
        setTitleI MsgUser
        $(widgetFile "blocks/admin/user_view")

getUserEditR :: UserId -> Handler Html
getUserEditR userId = do
    user            <- runDB $ get404 userId
    (form, encType) <- generateFormPost $ userEditForm user

    Layout.singleLarge "user-edit" $ do
        setTitleI MsgEditUser
        $(widgetFile "blocks/admin/user_edit")

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
        redirect UserListR

getUserCreateR :: Handler Html
getUserCreateR = do
    (form, encType) <- generateFormPost userCreateForm 

    Layout.singleLarge "user-create" $ do
        setTitleI MsgCreateUser
        $(widgetFile "blocks/admin/user_create")

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
        redirect UserListR

postUserRemoveConfirmR :: UserId -> Handler Html
postUserRemoveConfirmR userId = do
    user <- runDB $ get404 userId

    Layout.singleLarge "user-remove-confirm" $ do
        setTitleI MsgRemoveUser
        $(widgetFile "blocks/admin/user_remove_confirm")

postUserRemoveR :: UserId -> Handler Html
postUserRemoveR userId
    = if userId == firstUserId
      then do
          setAlertI Danger MsgCantDeleteRoot
          redirect UserListR
      else do
          runDB $ delete userId

          setAlertI Success MsgUserRemoved
          redirect UserListR

------------------------------------------------------------------------
-- * Forms

data UserEdit 
    = UserEdit Text (Maybe Text) Bool
      deriving Show

data UserCreate
    = UserCreate Text Bool
      deriving Show

userEditForm :: User -> Form UserEdit
userEditForm user = Layout.renderForm $ UserEdit
    <$> areq Layout.bs3StaticTextField (bfs MsgEmail) emailM
    <*> aopt passwordConfirmField (bfs MsgNewPassword) Nothing
    <*> areq Layout.bs3BoolField (bfs MsgIsAdmin) adminM
    <*  bootstrapSubmit (BootstrapSubmit MsgSave "btn-success" [])
  where
    emailM = Just (userEmail user)
    adminM = Just (userAdmin user)

userCreateForm :: Form UserCreate
userCreateForm = Layout.renderForm $ UserCreate
    <$> areq emailField (bfs MsgEmail) Nothing
    <*> areq Layout.bs3BoolField (bfs MsgIsAdmin) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgSave "btn-success" [])

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

firstUserId :: UserId
firstUserId = toSqlKey 1
