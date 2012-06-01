module OAuthToken
  ( OAuthToken
  , mkOAuthToken
  , getToken
  ) where

import Prelude
import Yesod
import qualified Data.Text as T
import Database.Persist.Store
  ( SqlType (SqlString)
  , PersistValue (PersistText)
  )

newtype OAuthToken = Token T.Text deriving (Eq)

instance Show OAuthToken where
  show (Token tok) = show tok

getToken :: OAuthToken -> T.Text
getToken (Token tok) = tok

-- TODO: this needs to be tested with unit tests
mkOAuthToken :: String -> Maybe OAuthToken
mkOAuthToken text = if correctLength && validChars text
                    then Just $ Token $ T.pack text
                    else Nothing
  where
    correctLength = length text == 16

    validChars = foldr ((&&) . base64Char) True

    base64Char x = or [ x `elem` ['A' .. 'Z']
                      , x `elem` ['a' .. 'z']
                      , x `elem` ['0' .. '9']
                      , x `elem` "+/=" ]

instance PathPiece OAuthToken where
  fromPathPiece s =
    case reads $ T.unpack s of
      [(a, "")] -> Just a
      _         -> Nothing

  toPathPiece s = T.pack $ show s

instance Read OAuthToken where
  readsPrec _ s = case mkOAuthToken s of
                    Just tok -> [(tok, "")]
                    Nothing  -> []

instance PersistField OAuthToken where
  sqlType _            = SqlString

  toPersistValue token = PersistText $ getToken token

  fromPersistValue (PersistText val)
                      = case mkOAuthToken $ T.unpack val of
                          Just tok -> Right tok
                          Nothing  -> Left "no token"
  fromPersistValue _  = Left "unsupported value"
