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
                [TextPageSlug ==. (textPageSlug tmpPage1)]

            firstPageM <- runDB $ TextPageM.getFirst

            liftIO $ case firstPageM of
                Nothing
                    -> assertFailure "no page returned"
                Just (Entity _ resPage)
                    -> assertEqual "is expected page" tmpPage2 resPage
    describe "getPaginated" $ do
        it "getting first page of pages" $ do
            addAll

            (_, pages) <- runDB $ TextPageM.getPaginated 0

            liftIO $ assertEqual "is right amount of pages" 4 (length pages)
        it "getting out of bound of pages" $ do
            addAll

            (_, pages) <- runDB $ TextPageM.getPaginated 100

            case pages of
                [] -> return ()
                _  -> liftIO $ assertFailure "invalid amount of pages retrieved"
    describe "getPublic" $ do
        it "using 2 public pages" $ do
            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2

            pages <- runDB $ TextPageM.getPublic

            liftIO $ assertEqual "got 2 pages back" 2 (length pages) 
        it "using 1 public page and 1 private page" $ do
            runDB $ do
                void $ insert tmpPage1
                void $ insert tmpPage2{ textPagePublic = False }

            pages <- runDB $ TextPageM.getPublic

            liftIO $ assertEqual "got 1 page back" 1 (length pages) 
    describe "getCurrPublic" $ do
        it "using 1 public page" $ do
            runDB $ do
                void $ insert tmpPage1

            pageM <- runDB $ TextPageM.getCurrPublic (textPageSlug tmpPage1)

            liftIO $ case pageM of
                Nothing
                    -> assertFailure "no page returned"
                Just (Entity _ resPage)
                    -> assertEqual "is expected page" tmpPage1 resPage
        it "using 1 private page" $ do
            runDB $ do
                void $ insert tmpPage1{ textPagePublic = False }

            pageM <- runDB $ TextPageM.getCurrPublic (textPageSlug tmpPage1)

            liftIO $ case pageM of
                Nothing
                    -> return ()
                _
                    -> assertFailure "private page was returned"
  where
    tmpPage1 = TextPage "Test page 1" "test-page1" "" True Nothing
    tmpPage2 = TextPage "Test page 2" "test-page2" "" True Nothing
    tmpPage3 = TextPage "Test page 3" "test-page3" "" True Nothing
    tmpPage4 = TextPage "Test page 4" "test-page4" "" True Nothing
    tmpPages = [tmpPage1, tmpPage2, tmpPage3, tmpPage4]
    addAll   = runDB $ forM_ tmpPages (void . insert)
