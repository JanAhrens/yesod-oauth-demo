module OAuthToken
  ( OAuthToken
  , mkOAuthToken
  ) where

import Prelude
import qualified Data.Text as T

newtype OAuthToken = Token T.Text deriving (Eq)

instance Show OAuthToken where
  show (Token t) = show t

-- TODO: this needs to be tested with unit tests
mkOAuthToken :: String -> OAuthToken
mkOAuthToken text = if correctLength && validChars text
                    then Token $ T.pack text
                    else error "invalid token"
  where
    correctLength = length text == 16

    validChars = foldr ((&&) . base64Char) True

    base64Char x = or [ x `elem` ['A' .. 'Z']
                      , x `elem` ['a' .. 'z']
                      , x `elem` ['0' .. '9']
                      , x `elem` "+/=" ]
