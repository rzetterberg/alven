module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.Quasi
import           Text.Markdown
import           Yesod.Text.Markdown()

-------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


instance Eq TextPage where
    (==) a b = (textPagePermalink a) == (textPagePermalink b)
    (/=) a b = not (a == b)
