module OAuthToken
( AccessToken
, RequestToken
, OAuthToken
) where

import Prelude
import Yesod
import qualified Data.Text as T
import Database.Persist.Store
  ( SqlType (SqlString)
  , PersistValue (PersistText)
  )

class (Read a, Show a, PathPiece a, PersistField a) => OAuthToken a where
  mkToken  :: String -> Maybe a
  getToken :: a -> T.Text

newtype AccessToken  = AccessToken  T.Text deriving (Eq)
newtype RequestToken = RequestToken T.Text deriving (Eq)

instance Show AccessToken where
  show (AccessToken t) = T.unpack t

instance Show RequestToken where
  show (RequestToken t) = T.unpack t

instance OAuthToken RequestToken where
  mkToken = mkOAuthToken RequestToken "R-"
  getToken (RequestToken t) = t

instance OAuthToken AccessToken where
  mkToken = mkOAuthToken AccessToken "A-"
  getToken (AccessToken t) = t

simpleReadsPrec :: (OAuthToken t) => Int -> ReadS t
simpleReadsPrec _ s = case mkToken s of
                          Just tok -> [(tok, "")]
                          Nothing  -> []

instance Read AccessToken where
  readsPrec = simpleReadsPrec

instance Read RequestToken where
  readsPrec = simpleReadsPrec

mkOAuthToken :: (OAuthToken a) => (T.Text -> a) -> String -> String -> Maybe a
mkOAuthToken constructor pre text = if correctLength && validChars text && prefixMatches
                    then Just $ constructor $ T.pack text
                    else Nothing
  where
    length_without_prefix = 16

    correctLength = length text == length_without_prefix + length pre

    validChars = foldr ((&&) . base64Char) True

    prefixMatches = take (length pre) text == pre

    base64Char x = or [ x `elem` ['A' .. 'Z']
                      , x `elem` ['a' .. 'z']
                      , x `elem` ['0' .. '9']
                      , x `elem` "+/="
                      , x `elem` pre]

generalFromPathPiece :: OAuthToken a => T.Text -> Maybe a
generalFromPathPiece s =
    case reads $ T.unpack s of
      [(a, "")] -> Just a
      _         -> Nothing

instance PathPiece RequestToken where
  fromPathPiece = generalFromPathPiece
  toPathPiece   = T.pack . show

instance PathPiece AccessToken where
  fromPathPiece = generalFromPathPiece
  toPathPiece   = T.pack . show


instance PersistField RequestToken where
  sqlType _            = SqlString

  toPersistValue = PersistText . getToken

  fromPersistValue (PersistText val)
                      = case mkToken $ T.unpack val of
                          Just tok -> Right tok
                          Nothing  -> Left "no token"
  fromPersistValue _  = Left "unsupported value"

instance PersistField AccessToken where
  sqlType _            = SqlString

  toPersistValue = PersistText . getToken

  fromPersistValue (PersistText val)
                      = case mkToken $ T.unpack val of
                          Just tok -> Right tok
                          Nothing  -> Left "no token"
  fromPersistValue _  = Left "unsupported value"
