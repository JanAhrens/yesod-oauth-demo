{-# LANGUAGE OverloadedStrings #-}
module Model where

import Prelude
import Yesod
import Database.Persist.Quasi
import Data.Text (Text)
import qualified Data.Text as S
import OAuthToken

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance PathPiece OAuthToken where
  fromPathPiece s =
    case reads $ S.unpack s of
      [(a, "")] -> Just $ mkOAuthToken a
      _         -> Nothing

  toPathPiece s = S.pack $ show s

instance Read OAuthToken where
  readsPrec _ s = [(mkOAuthToken s, "")]
