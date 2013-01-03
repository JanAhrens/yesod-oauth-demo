{-# LANGUAGE OverloadedStrings #-}
module RequestTokenTest
    ( requestTokenSpecs
    ) where

import TestImport

requestTokenSpecs :: Specs
requestTokenSpecs =

  describe "creating request tokens" $ do

    it "can successfully be created" $ do
      post_ "/request_token"
      statusIs 201

    it "confirms the callback domain" $ do
      post_ "/request_token"
      bodyContains "oauth_callback_confirmed=true"

    it "returns oauth token and secret" $ do
      post_ "/request_token"
      bodyContains "oauth_token="
      bodyContains "oauth_token_secret="
