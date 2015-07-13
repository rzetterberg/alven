module Foundation where

import           Import.NoFoundation
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Control.Monad.Logger (LoggingT, runLoggingT)
import           Text.Hamlet          (hamletFile)
import           Text.Lucius          (luciusFile)
import           Text.Jasmine         (minifym)
import           Yesod.Default.Util   (addStaticContentExternal)
import           Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import           Plugin.Auth.Email 

import           Layout.Component.Alert (getAlertT, Level(Danger), changeAlertLevelT)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout content = buildLayout $ do 
        toWidgetHead $(luciusFile "templates/layout/public/global.lucius")

        $(widgetFile "layout/public/default")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _
        = return Authorized
    isAuthorized PageHomeR _
        = return Authorized
    isAuthorized (PageViewR _) _
        = return Authorized
    isAuthorized FaviconR _
        = return Authorized
    isAuthorized RobotsR _
        = return Authorized
    isAuthorized _ _
        = maybeAuthId >>= \userM -> return $ case userM of
            Just _  -> Authorized
            Nothing -> AuthenticationRequired

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    authLayout content = do
        (currCrumb, parentCrumbs) <- breadcrumbs
        route  <- getCurrentRoute
        alertM <- getAlertT >>= \alert -> return $ case route of
            Just (AuthR LoginR) -> changeAlertLevelT Danger alert
            _                   -> alert

        buildLayout $ do 
            let alert          = $(widgetFile "components/alert")
                pageIdentifier = "login" :: Text

            toWidgetHead $(luciusFile "templates/layout/public/global.lucius")

            $(widgetFile "layout/public/auth")

    -- Where to send a user after successful login
    loginDest _ = AdminR
    -- Where to send a user after logout
    logoutDest _ = AuthR LoginR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser (credsIdent creds)

        return $ case x of
            Just (Entity uid _) -> Just uid
            Nothing             -> Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    addUnverified email verkey = runDB $ insert $ 
        User email Nothing (Just verkey) False False

    sendVerifyEmail _ _ vurl = liftIO $ do
        putStrLn "================="
        print vurl 
        putStrLn "================="

    getVerifyKey uid = runDB (get uid) >>= return . join . fmap userVerkey

    setVerifyKey uid vkey = runDB $ update uid [UserVerkey =. Just vkey]

    verifyAccount uid = runDB $ get uid >>= \userM -> case userM of
        Just _  -> update uid [UserVerified =. True] >> return (Just uid)
        Nothing -> return Nothing

    getPassword uid = runDB (get uid) >>= return . join . fmap userPassword

    setPassword uid spass = runDB $ get uid >>= \userM -> case userM of
        Just _  -> update uid [UserPassword =. Just spass] 
        Nothing -> return ()

    getEmailCreds email = do
        userM <- runDB $ getBy (UniqueUser email)

        return $ case userM of
            Nothing                -> Nothing
            Just (Entity uid user) -> Just EmailCreds
                { emailCredsId     = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust (userPassword user)
                , emailCredsVerkey = userVerkey user
                , emailCredsEmail  = email
                }

    getEmail uid = runDB (get uid) >>= return . fmap userEmail

    afterPasswordRoute _ = AdminR

    setPasswordHandler needOld = do
        tp <- getRouteToParent
        mr <- lift getMessageRender
        selectRep $ do
            provideJsonMessage $ mr MsgSetPass
            provideRep $ lift $ authLayout $ do
                setTitleI MsgSetPassTitle
                [whamlet|
$newline never
<h3>_{MsgSetPass}
<form method="post" action="@{tp setpassR}">
  <table>
    $if needOld
      <tr>
        <th>_{MsgCurrentPassword}
        <td>
          <input type="password" name="current" autofocus>
    <tr>
      <th>_{MsgNewPass}
      <td>
        <input type="password" name="new" :not needOld:autofocus>
    <tr>
      <th>_{MsgConfirmPass}
      <td>
        <input type="password" name="confirm">
    <tr>
      <th>
      <td>
        <button type="submit" .btn .btn-success>_{MsgSetPassTitle}
|]

    forgotPasswordHandler = do
        tp <- getRouteToParent
        lift $ authLayout $ do
            setTitleI MsgPasswordResetTitle
            [whamlet|
$newline never
<p>_{MsgPasswordResetPrompt}
<form method="post" action="@{tp forgotPasswordR}">
  <table>
    <tr>
      <th>_{MsgEmail}
      <td>
        <input type="email" name="email" autofocus>
    <tr>
      <th>
      <td>
        <button type="submit" .btn .btn-success>_{MsgSendPasswordResetEmail}
|]

    registerHandler = do
         tp <- getRouteToParent
         lift $ authLayout $ do
             setTitleI MsgRegisterLong
             [whamlet|
$newline never
<p>_{MsgEnterEmail}
<form method="post" action="@{tp registerR}">
  <table>
    <tr>
      <th>_{MsgEmail}
      <td>
        <input type="email" name="email" autofocus>
    <tr>
      <th>
      <td>
        <button type="submit" .btn .btn-success>_{MsgRegister} 
|]

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodBreadcrumbs App where
    breadcrumb r = do
        msgRender <- getMessageRender

        let returnI (msg, path) = return (msgRender msg, path)

        returnI $ case r of
            AdminR
                -> (MsgAdmin, Nothing)
            PageListFirstR
                -> (MsgPages, Just AdminR)
            (PageListR _)
                -> (MsgPages, Just AdminR)
            PageCreateR
                -> (MsgCreatePage, Just PageListFirstR)
            (PageViewR _)
                -> (MsgViewPage, Just PageListFirstR)
            (PageEditR _)
                -> (MsgEditPage, Just PageListFirstR)
            (PageRemoveConfirmR _)
                -> (MsgRemovePage, Just PageListFirstR)
            (UserListR _)
                -> (MsgUsers, Just AdminR)
            UserListFirstR
                -> (MsgUsers, Just AdminR)
            UserCreateR
                -> (MsgCreateUser, Just UserListFirstR)
            (UserViewR _)
                -> (MsgViewUser, Just UserListFirstR)
            (UserEditR _)
                -> (MsgEditUser, Just UserListFirstR)
            (UserRemoveConfirmR _)
                -> (MsgRemoveUser, Just UserListFirstR)
            ThemeIndexR
                -> (MsgTheme, Just AdminR)
            ThemeAPIReferenceR
                -> (MsgAPIReference, Just ThemeIndexR)
            ThemeCompabilityCheckR
                -> (MsgAPIReference, Just ThemeIndexR)
            (AuthR _)
                -> (MsgAuthentication, Nothing)
            _
                -> (MsgUnknown, Just AdminR)

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

buildLayout :: WidgetT App IO () -> HandlerT App IO Html
buildLayout widget = do
    pc <- widgetToPageContent $ do
        $(combineScripts 'StaticR [js_jquery_min_js])
        $(combineStylesheets 'StaticR [css_bootstrap_css])
        widget

    withUrlRenderer $(hamletFile "templates/base.hamlet")

type DBRunnerIO =  forall (m :: * -> *) a. (MonadIO m, MonadBaseControl IO m)
                => SqlPersistT (Control.Monad.Logger.LoggingT m) a
                -> m a

runDBIO :: App -> DBRunnerIO
runDBIO master@App{..} action = do
    let logFunc = messageLoggerSource master appLogger

    runLoggingT (runSqlPool action appConnPool) logFunc
