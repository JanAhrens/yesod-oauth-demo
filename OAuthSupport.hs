module OAuthSupport where

import Import

import Control.Monad (liftM)
import qualified Data.Map as Map

data OAuthParams = OAuthParams
  { token           :: Text,
    consumer        :: Text,
    version         :: Text,
    signature       :: Text,
    signatureMethod :: Text,
    timestamp       :: Maybe Text,
    nonce           :: Maybe Text
    }

toOAuthHandler :: Handler a -> Handler a
toOAuthHandler handler = do
  e <- myRunInputGet $ OAuthParams
                    <$> ireq textField "oauth_token"
                    <*> ireq textField "oauth_consumer"
                    <*> ireq textField "oauth_version"
                    <*> ireq textField "oauth_signature"
                    <*> ireq textField "oauth_signature_method"
                    <*> iopt textField "oauth_timestamp"
                    <*> iopt textField "oauth_nonce"
  case e of
    Right _ -> handler
    Left _  -> redirect NotesR

-- COPY AND PASTE CODE :'-(

type DText = [Text] -> [Text]
myRunInputGet :: FormInput sub master OAuthParams
              -> GHandler  sub master (Either DText OAuthParams)
myRunInputGet (FormInput f) = do
    env <- liftM (toMap . reqGetParams) getRequest
    m <- getYesod
    l <- languages
    emx <- f m l env
    return emx

toMap :: [(Text, a)] -> Map.Map Text [a]
toMap = Map.unionsWith (++) . map (\(x, y) -> Map.singleton x [y])
