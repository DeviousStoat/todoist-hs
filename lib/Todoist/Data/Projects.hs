module Todoist.Data.Projects where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

import Todoist.Data.Common (Color, ViewStyle, todoistParseJSON, todoistToJSON)

data Project = Project
  { id :: Text
  , name :: Text
  , color :: Color
  , parentId :: Maybe Text
  , order :: Int
  , commentCount :: Int
  , isShared :: Bool
  , isFavorite :: Bool
  , isInboxProject :: Bool
  , isTeamInbox :: Bool
  , viewStyle :: ViewStyle
  , url :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Project where
  parseJSON = todoistParseJSON

instance ToJSON Project where
  toJSON = todoistToJSON

data ProjectPostCreate = ProjectPostCreate
  { name :: Text
  , parentId :: Maybe Text
  , color :: Maybe Color
  , isFavorite :: Maybe Bool
  , viewStyle :: Maybe ViewStyle
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProjectPostCreate where
  parseJSON = todoistParseJSON

instance ToJSON ProjectPostCreate where
  toJSON = todoistToJSON

data ProjectPostUpdate = ProjectPostUpdate
  { name :: Maybe Text
  , color :: Maybe Color
  , isFavorite :: Maybe Bool
  , viewStyle :: Maybe ViewStyle
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProjectPostUpdate where
  parseJSON = todoistParseJSON

instance ToJSON ProjectPostUpdate where
  toJSON = todoistToJSON

data Collaborator = Collaborator
  { id :: Text
  , name :: Text
  , email :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Collaborator where
  parseJSON = todoistParseJSON

instance ToJSON Collaborator where
  toJSON = todoistToJSON
