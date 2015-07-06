{-# LANGUAGE RecordWildCards #-}

{-|
Functionality for alert handling that supports different types of alerts.

Can be used to display error, success, warning and info message boxes in
conjunction with form handling.

Supports supplying the message as Text or a translation type.

Saves alert messages as session data that is cleared once shown to the client.
-}
module Layout.Component.Alert 
    ( Level(..)
    , Alert(..)
    , setAlert
    , setAlertI
    , getAlert
    , getAlertT) where

import           Prelude               
import           Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextLE
import qualified Data.Text.Encoding as TextE
import qualified Data.Aeson as Aeson
import           Data.Aeson.TH
import           Text.Blaze (ToMarkup(toMarkup))
import           Yesod hiding (setMessage, getMessage)

------------------------------------------------------------------------

data Level
    = Success 
    | Info 
    | Warning
    | Danger
      deriving Show

instance ToMarkup Level where
    toMarkup = toMarkup . textLevel
      where
        textLevel :: Level -> Text
        textLevel lvl = case lvl of
            Success -> "alert-success"
            Info    -> "alert-info"
            Warning -> "alert-warning"
            Danger  -> "alert-danger"

data Alert = Alert
    { alertLevel :: Level 
    , alertMsg   :: Text
    } deriving Show

$(deriveJSON defaultOptions ''Level)
$(deriveJSON defaultOptions ''Alert)

sessKey :: Text
sessKey = "_ALERT"

setAlertI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
          => Level 
          -> msg
          -> m ()
setAlertI lvl msg 
    = getMessageRender >>= \renderer -> setAlert lvl $ renderer msg

setAlert :: MonadHandler m => Level -> Text -> m ()
setAlert lvl msg = setSession sessKey $ 
    toSessionVal Alert{alertLevel = lvl, alertMsg = msg}
  where
    toSessionVal = TextL.toStrict . TextLE.decodeUtf8 . Aeson.encode

getAlert :: MonadHandler m => m (Maybe Alert)
getAlert = do
    valM <- lookupSession sessKey
    deleteSession sessKey

    return $ case valM of
                Nothing  -> Nothing
                Just val -> Aeson.decodeStrict $ TextE.encodeUtf8 val

getAlertT :: MonadHandler m => m (Maybe (Html, Html))
getAlertT = do
    alertM <- getAlert

    return $ case alertM of
                Nothing        -> Nothing
                Just Alert{..} -> Just (toHtml alertLevel, toHtml alertMsg)
