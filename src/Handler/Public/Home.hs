{-|
Provides the handlers for the home page routes.
-}
module Handler.Public.Home where

import           Import

import           Handler.Public.Page (getPageViewR)

-------------------------------------------------------------------------------

{-|
Selects the first page in the database and runs 'getPageViewR' handler with the
page slug. If no page is in the database and empty string is used.
-}
getHomeR :: Handler Html
getHomeR = do
    page  <- runDB $ selectList [] [LimitTo 1]
    
    let pslug = case page of
                    []    -> ""  
                    (Entity _ p:_) -> textPagePermalink p

    getPageViewR pslug
