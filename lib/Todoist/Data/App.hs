module Todoist.Data.App where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (
  Manager,
  ManagerSettings (managerModifyRequest),
  applyBearerAuth,
  newManager,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Env = Env
  { apiBaseUrl :: String
  , httpManager :: Manager
  }

mkEnv :: Text -> IO Env
mkEnv token = Env baseUrl <$> manager
 where
  baseUrl :: String
  baseUrl = "https://api.todoist.com/api/v1"

  manager :: IO Manager
  manager =
    newManager $
      tlsManagerSettings
        { managerModifyRequest = pure . applyBearerAuth (encodeUtf8 token)
        }

type TodoistApp = ReaderT Env IO
