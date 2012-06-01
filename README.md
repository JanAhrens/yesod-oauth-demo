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
* Draft for request token authorization
* Use OAuthToken datatype in database

What needs to be done
---------------------

* Authorize a request token
* Exchange request token for access token
* OAuth signature validation (oauth_signature, oauth_signature_method)
* OAuth parameter validation (oauth_token, oauth_consumer_key, oauth_version)
* Example API call to test the implementation and workflow

What is not part of the initial implementation
----------------------------------------------

* Consumer management (they need to be manually managed in the database)
* Permission handling (the user can only give access to all of his data)
