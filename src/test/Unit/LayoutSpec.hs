{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unit.LayoutSpec (spec) where

import           Data.Char (isLetter, isNumber)
import qualified Data.Text as T
import           TestImport
import           Data.List ((!!))

import           Layout.Util (removeClass)

-------------------------------------------------------------------------------

spec :: Spec
spec = describe "removeClass" $ do
    it "removes with single class" $
        property prop_checkRemove_single
    it "removes with multiple classes" $
        property prop_checkRemove_multi

instance Arbitrary Text where
    arbitrary = fromString <$> listOf1 (suchThat (arbitrary :: Gen Char) constraint)
      where
        constraint c = isLetter c || isNumber c || c == '-'
    
{-|
Property that makes sure the added class is removed when using a single class.
-}
prop_checkRemove_single :: Text           -- ^ Class to add and remove
                        -> [(Text, Text)] -- ^ All other attributes
                        -> Bool
prop_checkRemove_single classToRemove based
    = (removeClass classToRemove inpAttrs) == outpAttrs
  where
    inpAttrs  = ("class", classToRemove) : based
    outpAttrs = ("class", "") : based

{-|
Property that makes sure the added class is removed when using multiple classes.

Takes a list of classes and a random positive number. The random number is
modulated to an index of a class inside the class list. The class that has that
index is passed in to 'removeClass' as the class to be removed.
-}
prop_checkRemove_multi :: Positive Int    -- ^ Index of class to test removal of
                       -> [Text]          -- ^ All other class names
                       -> [(Text, Text)]  -- ^ All other attributes
                       -> Bool
prop_checkRemove_multi (Positive randIndex) inpClasses otherAttrs
    = (removeClass classToRemove inpAttrs) == outpAttrs
  where
    inpAttrs      = ("class", joinAttrVals inpClasses)  : otherAttrs
    outpAttrs     = ("class", joinAttrVals outpClasses) : otherAttrs
    outpClasses   = filter (/= classToRemove) inpClasses
    classIndex'   = (length inpClasses) `mod` randIndex
    classIndex    = if classIndex' == 0
                    then classIndex'
                    else classIndex' - 1
    classToRemove = if null inpClasses
                    then ""
                    else inpClasses !! classIndex

-------------------------------------------------------------------------------
-- * Utils

{-|
Takes a list of HTML tag attribute values and joins them together. Can be used
to join a list of classes to give a tag.

>>> joinAttrVals ["btn", "btn-default"]
"btn btn-default"

>>> joinAttrVals [" btn", " btn-lg"]
"btn btn-lg"
-}
joinAttrVals :: [Text] -> Text
joinAttrVals = T.intercalate " " . map T.strip
