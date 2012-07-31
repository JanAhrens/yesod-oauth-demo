module OAuthSupport where

import Import

import Control.Monad (liftM)
import qualified Data.Map as Map
import Web.Authenticate.OAuth

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
  res <- myRunInputGet $ OAuthParams
                    <$> ireq textField "oauth_token"
                    <*> ireq textField "oauth_consumer"
                    <*> ireq textField "oauth_version"
                    <*> ireq textField "oauth_signature"
                    <*> ireq textField "oauth_signature_method"
                    <*> iopt textField "oauth_timestamp"
                    <*> iopt textField "oauth_nonce"
  case res of
    Right _ -> do
      -- TOOD get real credentials
      let tok = newCredential "foo" "bar"
      -- TODO full OAuth type
      let oa = def
      -- TODO use this to construct `req'
      let origRequest = getRequest
      let req = def
      -- TODO compare `res' with `sign'
      sign <- genSign oa tok req
      handler
    -- TODO implement propper error handling
    Left _  -> redirect NotesR

-- TODO here follows copy and paste code: find out how to reuse or improve

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
