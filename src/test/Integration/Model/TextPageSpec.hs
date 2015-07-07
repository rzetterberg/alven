module Integration.Model.TextPageSpec (spec) where

import           TestImport hiding (assertEqual)
import           Test.HUnit (assertEqual, assertFailure)

import qualified Model.TextPage as TextPageM

-------------------------------------------------------------------------------

spec :: Spec
spec = withApp $ do
    describe "getFirstPage" $ do
        it "1 page exist, that page should be returned" $ do
            runDB $ do
                void $ insert tmpPage1

            firstPageM <- runDB $ TextPageM.getFirst

            liftIO $ case firstPageM of
                Nothing
                    -> assertFailure "no page returned"
                Just (Entity _ resPage)
                    -> assertEqual "is expected page" tmpPage1 resPage
        it "2 pages exists, first one should be returned" $ do
            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            firstPageM <- runDB $ TextPageM.getFirst

            liftIO $ case firstPageM of
                Nothing
                    -> assertFailure "no page returned"
                Just (Entity _ resPage)
                    -> assertEqual "is expected page" tmpPage1 resPage
        it "first page is removed, second one should be returned" $ do
            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            runDB $ deleteWhere
                [TextPagePermalink ==. (textPagePermalink tmpPage1)]

            firstPageM <- runDB $ TextPageM.getFirst

            liftIO $ case firstPageM of
                Nothing
                    -> assertFailure "no page returned"
                Just (Entity _ resPage)
                    -> assertEqual "is expected page" tmpPage2 resPage
    describe "getPaginated" $ do
        it "getting lower half of pages" $ do
            addAll

            pages <- runDB $ TextPageM.getPaginated 0 2

            return $ case pages of
                ((Entity _ a):(Entity _ b):[]) -> do
                    assertEqual "is first page" tmpPage1 a
                    assertEqual "is second page" tmpPage2 b
                _ -> assertFailure "invalid amount of pages retrieved"
        it "getting upper half of pages" $ do
            addAll

            pages <- runDB $ TextPageM.getPaginated 2 2

            return $ case pages of
                ((Entity _ c):(Entity _ d):[]) -> do
                    assertEqual "is third page" tmpPage3 c
                    assertEqual "is fourth page" tmpPage4 d
                _ -> assertFailure "invalid amount of pages retrieved"
        it "getting middle half of pages" $ do
            addAll

            pages <- runDB $ TextPageM.getPaginated 2 2

            return $ case pages of
                ((Entity _ b):(Entity _ c):[]) -> do
                    assertEqual "is second page" tmpPage2 b
                    assertEqual "is third page" tmpPage3 c
                _ -> assertFailure "invalid amount of pages retrieved"
  where
    tmpPage1 = TextPage "Test page 1" "test-page1" "" True Nothing
    tmpPage2 = TextPage "Test page 2" "test-page2" "" True Nothing
    tmpPage3 = TextPage "Test page 3" "test-page3" "" True Nothing
    tmpPage4 = TextPage "Test page 4" "test-page4" "" True Nothing
    tmpPages = [tmpPage1, tmpPage2, tmpPage3, tmpPage4]
    addAll   = runDB $ forM_ tmpPages (void . insert)
