{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.OAuth where

import Import

import qualified Network.HTTP.Types as H
import Network.Wai

-- OAuth step 1, fetch a request token
postRequestTokenR :: Handler RepPlain
postRequestTokenR =
  return $ RepPlain $ toContent ("oauth_token=&oauth_token_secret=&oauth_callback_confirmed=true" :: ByteString)

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
