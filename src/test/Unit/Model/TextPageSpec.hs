{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unit.Model.TextPageSpec (spec) where

import           TestImport 

import           Model.TextPage ( pageToOffset, pageToLimit, lenToLastPage
                                , itemsPerPage)

-------------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "pageToOffset" $ do
        it "page number 0" $ do
            (pageToOffset 0) `shouldBe` 0
        it "page number 1" $ do
            (pageToOffset 1) `shouldBe` itemsPerPage
    describe "pageToLimit" $ do
        it "page number 0" $ do
            (pageToLimit 0) `shouldBe` itemsPerPage
        it "page number 1" $ do
            (pageToLimit 1) `shouldBe` (itemsPerPage + itemsPerPage)
    describe "lenToLastPage" $ do
        it "0 pages" $ do
            (lenToLastPage 0) `shouldBe` 0
        it "above 0, under items per page" $ do
            (lenToLastPage (itemsPerPage - 1)) `shouldBe` 0
        it "equal to items per page" $ do
            (lenToLastPage itemsPerPage) `shouldBe` 0
        it "above items per page" $ do
            (lenToLastPage (itemsPerPage + 1)) `shouldBe` 1
