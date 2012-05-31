yesod-oauth-demo
================

This is an experiment to create a working OAuth 1.0 provider
implementation (as described in [RFC 5849](http://tools.ietf.org/html/rfc5849)) for [Yesod 1.0](https://github.com/yesodweb/yesod).

If you want to help me by improving my code or making sugesting, feel free to use GitHub pull requests and issues.

Files to look at
----------------

* [config/routes](https://github.com/JanAhrens/yesod-oauth-demo/blob/master/config/routes)
* [Handler/OAuth.hs](https://github.com/JanAhrens/yesod-oauth-demo/blob/master/Handler/OAuth.hs)
* [OAuthToken.hs](https://github.com/JanAhrens/yesod-oauth-demo/blob/master/OAuthToken.hs)

What works already
------------------

* Basic OAuth endpoint routing
* Random request token generation
* Custom datatype for OAuth tokens

What needs to be done
---------------------

* Authorize a request token
* Exchange request token for access token
* Store OAuthToken datatype in database

What is considered advanced stuff
---------------------------------

* OAuth consumer management
* Permission management