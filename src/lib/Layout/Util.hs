{-|
Utilities that contains generic functionality that is needed by layout modules.
-}
module Layout.Util where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude

-------------------------------------------------------------------------------

{-|
Removes a class name from the given list of attributes a HTML-tag has.

To represent a HTML-tags' attributes a list of tuples are used in yesod-form.
Say we have link we want to remove the CSS class "btn-default" from

@
<a href="http://google.com" class="btn btn-default">Google</a>
@

This tag would be represented as

@
let glink = [("href", "http://google.com"), ("class", "btn btn-default")]
@

We can remove "btn-default" by calling this function like so:

>>> removeClass "btn-default" glink
[("href", "http://google.com"), ("class", "btn")]
-}
removeClass :: Text           -- ^ Class to remove
            -> [(Text, Text)] -- ^ The tag attributes
            -> [(Text, Text)]
removeClass "" a                      = a
removeClass _  []                     = []
removeClass klass (("class", v):rest)
    = let klasses = T.splitOn " " v
          outp    = T.strip $ T.intercalate " " $ filter (/= klass) klasses
      in ("class", outp) : rest
removeClass klass (other:rest)        = other : removeClass klass rest
