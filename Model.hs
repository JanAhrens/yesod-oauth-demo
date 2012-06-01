{-# LANGUAGE OverloadedStrings #-}
module Model where

import Prelude
import Yesod
import Database.Persist.Quasi
import Data.Text (Text)
import OAuthToken

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
