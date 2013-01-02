{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport

homeSpecs :: Specs
homeSpecs =
  describe "Homepage" $
    it "display the documentation correctly" $ do
      get_ "/"
      statusIs 200
      htmlAllContain "h1" "Documentation"
