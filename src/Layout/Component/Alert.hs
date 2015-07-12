{-# LANGUAGE RecordWildCards #-}

{-|
Functionality for alert handling that supports different types of alerts.

Can be used to display error, success, warning and info message boxes in
conjunction with form handling.

Supports supplying the message as Text or a translation type.

Saves alert messages as session data that is cleared once shown to the client.

Leverages the built-in Yesod 'getMessage'/'setMessage' functionality and builds
on top of it by also supplying the severity of the message saved.
-}
module Layout.Component.Alert where

import           Prelude               
import           Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextLE
import qualified Data.Text.Encoding as TextE
import qualified Data.Aeson as Aeson
import           Data.Aeson.TH
import           Text.Blaze (ToMarkup(toMarkup))
import           Yesod 

------------------------------------------------------------------------

{-|
The severity of the saved message
-}
data Level
    = Success 
    | Info 
    | Warning
    | Danger
      deriving Show

instance ToMarkup Level where
    -- | Translate the severity to Bootstrap CSS classes
    toMarkup = toMarkup . textLevel
      where
        textLevel :: Level -> Text
        textLevel lvl = case lvl of
            Success -> "alert-success"
            Info    -> "alert-info"
            Warning -> "alert-warning"
            Danger  -> "alert-danger"

{-|
The carrier data type for a saved alert message
-}
data Alert = Alert
    { alertLevel :: Level 
    , alertMsg   :: Text
    } deriving Show

$(deriveJSON defaultOptions ''Level)
$(deriveJSON defaultOptions ''Alert)

{-|
Same key that is used in the built-in Yesod 'getMessage' and 'setMessage'
functionality.
-}
sessKey :: Text
sessKey = "_MSG"

{-|
Wrapper to 'setAlert' to allow usage of translatable messages. 
-}
setAlertI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
          => Level 
          -> msg
          -> m ()
setAlertI lvl msg 
    = getMessageRender >>= \renderer -> setAlert lvl $ renderer msg

{-|
Saves the given message as the given severity in the clients browser as
session data to be used in the next request.

The message is encoded as JSON before saving it to provide both the literal
message and the severity.
-}
setAlert :: MonadHandler m => Level -> Text -> m ()
setAlert lvl msg = setSession sessKey $ 
    toSessionVal Alert{alertLevel = lvl, alertMsg = msg}
  where
    toSessionVal = TextL.toStrict . TextLE.decodeUtf8 . Aeson.encode

{-|
Retrives the current message from the client and deletes the session data so
that it won't be displayed twice.

Tries to decode JSOn data into an 'Alert', if it fails, it just uses the session
data as message in a new 'Alert' that defaults 'Info' as alert level.
-}
getAlert :: MonadHandler m => m (Maybe Alert)
getAlert = do
    valM <- lookupSession sessKey
    deleteSession sessKey

    return $ case valM of
                Nothing  -> Nothing
                Just msg -> checkDecoding msg $
                                Aeson.decodeStrict $ TextE.encodeUtf8 msg
  where
    checkDecoding msg Nothing = Just Alert{ alertLevel = Info, alertMsg = msg }
    checkDecoding _ alertM    = alertM

{-|
Wrapper for 'getAlert' to retrive the message and severity as a tuple instead
of a 'Alert' data type.
-}
getAlertT :: MonadHandler m => m (Maybe (Html, Html))
getAlertT = do
    alertM <- getAlert

    return $ case alertM of
                Nothing        -> Nothing
                Just Alert{..} -> Just (toHtml alertLevel, toHtml alertMsg)

{-|
Changes the given alert tuples' alert level to the given one
-}
changeAlertLevelT :: Level -> Maybe (Html, Html) -> Maybe (Html, Html)
changeAlertLevelT lvl = fmap (\(_, msg) -> (toHtml lvl, msg))

{-|
Changes the given alert level to the given one
-}
changeAlertLevel :: Level -> Maybe Alert -> Maybe Alert
changeAlertLevel lvl = fmap (\a -> a{ alertLevel = lvl })
