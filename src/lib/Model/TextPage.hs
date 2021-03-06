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
Retrives a list of pages by 'defaultSort' for the current list page
-}
getPaginated :: (MonadIO m)
             => Int
             -> SqlPersistT m (Pagination, [Entity TextPage])
getPaginated pageNo = do
    pagesLen <- count ([] :: [Filter TextPage])

    let pagination@Pagination{..} = calcPagination pageNo pagesLen

    pages <- selectList [] [ OffsetBy paginationOffset
                           , LimitTo paginationLimit
                           , defaultSort
                           ]

    return (pagination, pages)

{-|
Retrives a list of public pages by 'defaultSort'.
-}
getPublic :: (MonadIO m) => SqlPersistT m [Entity TextPage]
getPublic = selectList [TextPagePublic ==. True] [defaultSort]

{-|
Retrives a page by the given slug, returns "Nothing" if the page is not found
or if it's private.
-}
getCurrPublic :: (MonadIO m)
              => Text
              -> SqlPersistT m (Maybe (Entity TextPage))
getCurrPublic slug = do
    matches <- selectList [ TextPagePublic ==. True
                          , TextPageSlug   ==. slug] []

    return $ case matches of
        (p:_) -> Just p
        _     -> Nothing

-------------------------------------------------------------------------------
-- * Constants

{-|
The default way to sort pages, currently sorts by ID ascending
-}
defaultSort :: SelectOpt TextPage
defaultSort = Asc TextPageId
