{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.OAuth where

import Import
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as TS

import qualified Network.HTTP.Types as H
import Network.Wai

import Network.Mail.Mime (randomString)
import System.Random (newStdGen)

-- OAuth step 1, fetch a request token
postRequestTokenR :: Handler RepPlain
postRequestTokenR = do
  token  <- liftIO randomKey
  secret <- liftIO randomKey
  let reqTok = RequestToken token secret "http://yourdomain.com/callback?params"
  _ <- runDB $ insert reqTok
  return $ RepPlain $ toContent $
      BS.append "oauth_token=" $ BS.append (T.encodeUtf8 $ requestTokenToken reqTok) $
      BS.append "&oauth_token_secret=" $ BS.append (T.encodeUtf8 $ requestTokenSecret reqTok)
                "&oauth_callback_confirmed=true"
  where
    randomKey = do
      stdgen <- newStdGen
      return $ TS.pack $ fst $ randomString 16 stdgen

-- OAuth step 2, get the authorization from the user
data Authorization = Authorization
                      { granted :: Bool}

authorizeForm :: Form Authorization
authorizeForm = renderDivs $ Authorization
  <$> areq checkBoxField "Please access my data" Nothing

getAuthorizeR :: OAuthToken -> Handler RepHtml
getAuthorizeR token = do
  (widget, enctype) <- generateFormPost authorizeForm
  defaultLayout [whamlet|
  <h1>Do you want to grant access to your data?
  <form method=post action=@{AuthorizeR token} encrype=#{enctype}>
    ^{widget}
    <input type="submit" value="Do as I say">
|]

postAuthorizeR :: OAuthToken -> Handler RepHtml
postAuthorizeR (OAuthToken _) = do
  ((result, _), _) <- runFormPost authorizeForm
  let doRedirect = case result of
                  FormSuccess authorization -> granted authorization
                  _                         -> False
  if doRedirect
    then sendWaiResponse $ responseLBS H.status303 [("Location", "http://www.github.com/")] "ask that dude"
    else defaultLayout [whamlet|
<p>Your data stays on our server.
|]

-- OAuth step 3, fetch the access token
postAccessTokenR :: OAuthToken -> Handler RepPlain
postAccessTokenR _ =
  return $ RepPlain $ toContent ("oauth_token=&oauth_token_secret=&ident=" :: ByteString)
