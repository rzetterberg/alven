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
-- * Utils

{-|
Takes the current page number and returns how many pages to skip when selecting
a range of pages.
-}
pageToOffset :: Int -> Int
pageToOffset pageNo = pageNo * itemsPerPage

{-|
Takes the current page number and returns how many pages to return when
selecting a range of pages.
-}
pageToLimit :: Int -> Int
pageToLimit pageNo = (pageNo + 1) * itemsPerPage

{-|
Takes the amount of pages and returns how many pages are needed to list them all
-}
lenToLastPage :: Int -> Int
lenToLastPage 0        = 0
lenToLastPage pagesLen = (pagesLen - 1) `div` itemsPerPage

-------------------------------------------------------------------------------
-- * Constants

{-|
The default way to sort pages, currently sorts by ID ascending
-}
defaultSort :: SelectOpt TextPage
defaultSort = Asc TextPageId

{-|
Amount of items to show on each page list page
-}
itemsPerPage :: Int
itemsPerPage = 20
