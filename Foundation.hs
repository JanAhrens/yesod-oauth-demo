module Foundation
    ( App (..)
    , Route (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store as DPS
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import OAuthToken

data App = App
    { settings      :: AppConfig DefaultEnv Extra
    , getLogger     :: Logger
    , getStatic     :: Static                                       -- ^ Settings for static file serving.
    , connPool      :: DPS.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConfig
    }

--mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- disable the session backend, to not send cookies with each API request
    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        DPS.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    authPlugins _ = [authBrowserId, authGoogleEmail]
    authHttpManager = httpManager

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
