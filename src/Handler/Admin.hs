module Handler.Admin where

import           Import

import qualified Layout.Admin as Layout

-------------------------------------------------------------------------------

getAdminR :: Handler Html
getAdminR = Layout.singleLarge $ do
    setTitleI MsgAdmin

    $(widgetFile "blocks/admin/index")
