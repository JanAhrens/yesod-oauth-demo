{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Handler.Notes where

import OAuthSupport
import Import
import Data.Aeson hiding (object)

instance ToJSON (Entity Note) where
  toJSON (Entity note_id (Note user subject content)) = object
    [ "id"      .= note_id
    , "user"    .= user
    , "subject" .= subject
    , "content" .= content]

noteForm :: Form Note
noteForm = renderDivs $ Note <$> aformM requireAuthId
                             <*> areq textField "Subject" Nothing
                             <*> areq textField "Content" Nothing

getNotesR :: Handler RepHtml
getNotesR = do
  db <- runDB (selectList [] [])
  (widget, enctype) <- generateFormPost noteForm
  defaultLayout $(whamletFile "templates/notes/index.hamlet")

postNotesR :: Handler RepHtml
postNotesR = do
  ((res, _), _) <- runFormPost noteForm
  case res of
    FormSuccess entry -> do
                            _ <- runDB $ insert entry
                            setMessage "Created"
                            redirect NotesR
    FormFailure _     -> do
                            setSession "subject" "a"
                            setSession "content" "b"
                            setMessage "Validation error. Try again."
                            redirect NotesR
    FormMissing       -> redirect NotesR

getRestNotesR :: Handler RepJson
getRestNotesR = toOAuthHandler $ do
  db <- runDB (selectList [] [])
  jsonToRepJson . asNoteEntities $ db
  where
    asNoteEntities :: [Entity Note] -> [Entity Note]
    asNoteEntities = id
