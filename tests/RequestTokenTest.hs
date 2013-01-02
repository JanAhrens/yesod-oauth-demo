{-# LANGUAGE OverloadedStrings #-}
module RequestTokenTest
    ( requestTokenSpecs
    ) where

import TestImport

requestTokenSpecs :: Specs
requestTokenSpecs =
  describe "RequestToken" $
    it "can successfully create request tokens" $ do
      post_ "/request_token"
      statusIs 200
