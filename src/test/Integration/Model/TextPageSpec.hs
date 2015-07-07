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
  where
    tmpPage1 = TextPage "Test page 1" "test-page1" "" True Nothing
    tmpPage2 = TextPage "Test page 2" "test-page2" "" True Nothing
