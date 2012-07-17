module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import OAuthToken

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
