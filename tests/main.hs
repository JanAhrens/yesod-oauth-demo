{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Application (makeFoundation)
import TestImport

import HomeTest
import RequestTokenTest
import AuthorizeTest

specs :: Specs
specs = do
  homeSpecs
  requestTokenSpecs
  authorizeSpecs

main :: IO ()
main = do
    conf <- loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    runTests app (connPool foundation) specs
