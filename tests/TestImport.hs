{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , runDB
    , Specs
    , OAuthToken
    ) where

import Yesod.Test
import Database.Persist.GenericSql
import OAuthToken

type Specs = SpecsConn Connection

runDB :: SqlPersist IO a -> OneSpec Connection a
runDB = runDBRunner runSqlPool
