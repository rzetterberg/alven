{-|
Provides common database operations related to the 'TextPage' data type.

All functions should be able to run with logging or without logging, so
SqlPersistM is not used.
-}
module Model.TextPage where

import           Import

-------------------------------------------------------------------------------

{-|
Retrives the first page created of the current pages.
-}
getFirst :: (MonadIO m)
         => SqlPersistT m (Maybe (Entity TextPage))
getFirst = do
    matches :: [Entity TextPage] <- selectList [] [LimitTo 1, defaultSort]

    return $ case matches of
        []    -> Nothing
        (p:_) -> Just p

{-|
Retrives a list of pages in ascending order by id within the given range
-}
getPaginated :: (MonadIO m)
             => Int                             -- ^ Pages to skip
             -> Int                             -- ^ Pages to get
             -> SqlPersistT m [Entity TextPage] -- ^ The paginated pages
getPaginated offset limit = selectList [] [ OffsetBy offset
                                          , LimitTo limit
                                          , defaultSort]

-------------------------------------------------------------------------------
-- * Constants

{-|
The default way to sort pages, currently sorts by ID ascending
-}
defaultSort :: SelectOpt TextPage
defaultSort = Asc TextPageId
