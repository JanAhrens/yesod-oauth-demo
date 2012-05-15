module Settings.StaticFiles where

import Prelude (IO)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)

staticSite :: IO Static.Static
staticSite =
#ifdef DEVELOPMENT
  Static.staticDevel staticDir
#else
  Static.static staticDir
#endif

$(staticFiles Settings.staticDir)
