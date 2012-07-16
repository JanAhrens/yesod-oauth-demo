{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import OAuthToken

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $(whamletFile "templates/documentation.hamlet")
  where
    sampleRequestToken :: RequestToken
    sampleRequestToken = read "R-ABCDEFGHIJKLMNOP"

    sampleVerifier = 12345
