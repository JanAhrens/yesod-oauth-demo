{-# LANGUAGE OverloadedStrings #-}
module Model where

import Prelude
import Yesod
import Database.Persist.Quasi
import Data.Text (Text)
import qualified Data.Text as S

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

newtype OAuthToken = OAuthToken Text deriving Eq

instance PathPiece OAuthToken where
  fromPathPiece s =
    case reads $ S.unpack s of
      [(a, "")] -> Just a
      _         -> Nothing

  toPathPiece (OAuthToken s) = s

instance Show OAuthToken where
  show (OAuthToken s) = show s

instance Read OAuthToken where
  readsPrec _ s | correctLength = if validChars s
                                    then [(OAuthToken $ S.pack s, "")]
                                    else failed
                | otherwise     = failed
    where
      correctLength = length s == 16
      failed = []

      validChars []     = True
      validChars (x:xs) = base64Char x && validChars xs

      base64Char x = or [ x `elem` ['A' .. 'Z']
                        , x `elem` ['a' .. 'z']
                        , x `elem` ['0' .. '9']
                        , x `elem` "+/=" ]
