{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unit.LayoutSpec (spec) where

import           Data.Char (isAlphaNum)
import qualified Data.Text as T
import           TestImport

import           Layout.Admin (removeClass)

-------------------------------------------------------------------------------

spec :: Spec
spec = describe "removeClass" $ do
    it "removes with single class" $ property prop_checkRemove_single
    it "removes with multiple classes" $ property prop_checkRemove_multi
  where

instance Arbitrary Text where
    arbitrary = fromString <$> listOf (suchThat (arbitrary :: Gen Char) isAlphaNum)
    
{-|
Property that makes sure the added class is removed when using a single class.
-}
prop_checkRemove_single :: Text -> [(Text, Text)] -> Bool
prop_checkRemove_single classn based = (removeClass classn inp) == outp
  where
    inp  = ("class", classn) : based
    outp = ("class", "")     : based

{-|
Property that makes sure the added class is removed when using multiple classes.
-}
prop_checkRemove_multi :: Text -> [Text] -> [(Text, Text)] -> Bool
prop_checkRemove_multi classn classes based = (removeClass classn inp) == outp
  where
    outpClasses = T.intercalate " " classes
    inpClasses  = T.intercalate " " [classn, outpClasses]
    inp  = ("class", inpClasses)  : based
    outp = ("class", outpClasses) : based
