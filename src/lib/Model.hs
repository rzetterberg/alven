module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.Quasi
import           Text.Markdown
import           Yesod.Text.Markdown()

-------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


instance Eq TextPage where
    (==) a b = (textPageSlug a) == (textPageSlug b)
    (/=) a b = not (a == b)

{-|
Represents the pagination of the current list page
-}
data Pagination = Pagination
    { paginationOffset   :: Int
    , paginationLimit    :: Int
    , paginationLastPage :: Int
    , paginationPrevPage :: Int
    , paginationNextPage :: Int
    } deriving (Show)

{-|
Calculates pagination for the current list page using page number and total
amount of items.

NOTE: page numbers start from 0
-}
calcPagination :: Int        -- ^ Current page number
               -> Int        -- ^ Total amount of items
               -> Pagination
calcPagination pageNo len = Pagination
    { paginationOffset   = pageToOffset pageNo
    , paginationLimit    = pageToLimit pageNo
    , paginationLastPage = lenToLastPage len
    , paginationPrevPage = pageNo - 1
    , paginationNextPage = pageNo + 1
    }

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
Amount of items to show on each page list page
-}
itemsPerPage :: Int
itemsPerPage = 20
