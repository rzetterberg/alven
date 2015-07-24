module Integration.Model.UserSpec (spec) where

import           TestImport hiding (assertEqual)
import           Test.HUnit (assertEqual, assertFailure)

import qualified Model.User as UserM

-------------------------------------------------------------------------------

spec :: Spec
spec = withApp $ do
    describe "getPaginated" $ do
        it "getting first page of users" $ do
            addAll

            (_, users) <- runDB $ UserM.getPaginated 0

            liftIO $ assertEqual "is right amount of users" 4 (length users)
        it "getting out of bound of users" $ do
            addAll

            (_, users) <- runDB $ UserM.getPaginated 100

            case users of
                [] -> return ()
                _  -> liftIO $ assertFailure "invalid amount of users retrieved"
  where
    tmpUser1 = User "tester1@test.com" Nothing Nothing False 
    tmpUser2 = User "tester2@test.com" Nothing Nothing False
    tmpUser3 = User "tester3@test.com" Nothing Nothing False
    tmpUser4 = User "tester4@test.com" Nothing Nothing False
    tmpUsers = [tmpUser1, tmpUser2, tmpUser3, tmpUser4]
    addAll   = runDB $ forM_ tmpUsers (void . insert)
