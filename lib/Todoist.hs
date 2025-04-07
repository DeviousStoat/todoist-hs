module Todoist where

import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Text qualified as Text
import System.Environment (getEnv)

import Todoist.Client (projects)
import Todoist.Data.App (mkEnv)

main :: IO ()
main = do
  token <- getEnv "TODOIST_API_TOKEN"
  env <- mkEnv (Text.pack token)
  p <- runReaderT projects env
  print p
