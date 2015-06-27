{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unit.LayoutSpec (spec) where

import           Data.Char (isLetter, isNumber)
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
    arbitrary = fromString <$> listOf (suchThat (arbitrary :: Gen Char) constraint)
      where
        constraint c = isLetter c || isNumber c || c == '-'
    
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
    inpClasses  = T.strip $ T.intercalate " " (classn : classes)
    outpClasses = T.strip $ T.intercalate " " classes
    inp  = ("class", inpClasses)  : based
    outp = ("class", outpClasses) : based
