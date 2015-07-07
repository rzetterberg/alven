{-|
Provides the handlers for the start page of the admin interface
-}
module Handler.Admin where

import           Import

import qualified Layout.Admin as Layout

-------------------------------------------------------------------------------

{-|
Displays the start page which contains links to the sub-modules.
-}
getAdminR :: Handler Html
getAdminR = Layout.singleLarge "admin-index" $ do
    setTitleI MsgAdmin

    $(widgetFile "blocks/admin/index")
