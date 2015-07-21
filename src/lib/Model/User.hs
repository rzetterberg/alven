{-|
Provides common database operations related to the 'User' data type.

All functions should be able to run with logging or without logging, so
SqlPersistM is not used.
-}
module Model.User where

import           Import

-------------------------------------------------------------------------------

{-|
Retrives a list of pages in ascending order by id for the current list page
-}
getPaginated :: (MonadIO m)
             => Int
             -> SqlPersistT m (Pagination, [Entity User])
getPaginated pageNo = do
    usersLen <- count ([] :: [Filter User])

    let pagination@Pagination{..} = calcPagination pageNo usersLen

    users <- selectList [] [ OffsetBy paginationOffset
                           , LimitTo paginationLimit
                           , defaultSort
                           ]

    return (pagination, users)

-------------------------------------------------------------------------------
-- * Constants

{-|
The default way to sort pages, currently sorts by ID ascending
-}
defaultSort :: SelectOpt User
defaultSort = Asc UserId
