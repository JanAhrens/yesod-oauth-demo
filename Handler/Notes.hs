{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Handler.Notes where

import Import
import Data.Aeson hiding (object)

instance ToJSON (Entity Note) where
  toJSON (Entity note_id (Note user subject content)) = object
    [ "id"      .= note_id
    , "user"    .= user
    , "subject" .= subject
    , "content" .= content]

getNotesR :: Handler RepHtmlJson
getNotesR = do
  db <- runDB (selectList [] [])
  defaultLayoutJson (html db)
                    (asNoteEntities db)
  where
    asNoteEntities :: [Entity Note] -> [Entity Note]
    asNoteEntities = id

    html dbList = [whamlet|
<ul>
  $forall Entity _ note <- dbList
    <li>#{noteSubject note}
      <p>
        #{noteContent note}
|]
