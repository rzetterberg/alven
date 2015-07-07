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
getFirst :: (MonadIO m) => SqlPersistT m (Maybe (Entity TextPage))
getFirst = do
    let ordering = [LimitTo 1, Asc TextPageId]
    matches :: [Entity TextPage] <- selectList [] ordering

    return $ case matches of
        []    -> Nothing
        (p:_) -> Just p
