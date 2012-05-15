module Settings
    ( widgetFile
    , PersistConfig
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite (SqliteConf)
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative

type PersistConfig = SqliteConf

staticDir :: FilePath
staticDir = "static"

staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]


widgetFile :: String -> Q Exp
#if DEVELOPMENT
widgetFile = Yesod.Default.Util.widgetFileReload
#else
widgetFile = Yesod.Default.Util.widgetFileNoReload
#endif

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"
