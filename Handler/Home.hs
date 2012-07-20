{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import OAuthToken

import qualified Data.Text as T

getHomeR :: Handler RepHtml
getHomeR = do
  muser <- maybeAuth
  defaultLayout $(whamletFile "templates/documentation.hamlet")
  where
    sampleRequestToken :: RequestToken
    sampleRequestToken = read $ T.unpack sampleRequestTokenString

    sampleRequestTokenString :: Text
    sampleRequestTokenString = "R-ABCDEFGHIJKLMNOP"

    sampleVerifier = "12345" :: Text
