{-# LANGUAGE DeriveAnyClass #-}

module Todoist.Data.Projects where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)
import Todoist.Data.Common (Color, ViewStyle, todoistParseJSON, todoistToJSON)

data Project = Project
  { id :: Text,
    parentId :: Maybe Text,
    order :: Maybe Int,
    color :: Color,
    name :: Text,
    isShared :: Bool,
    isFavorite :: Bool,
    isInboxProject :: Bool,
    isTeamInbox :: Bool,
    url :: Text,
    viewStyle :: ViewStyle,
    description :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Project where
  parseJSON = todoistParseJSON

instance ToJSON Project where
  toJSON = todoistToJSON

data ProjectPostCreate = ProjectPostCreate
  { name :: Text,
    description :: Maybe Text,
    parentId :: Maybe Text,
    color :: Maybe Color,
    isFavorite :: Maybe Bool,
    viewStyle :: Maybe ViewStyle
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProjectPostCreate where
  parseJSON = todoistParseJSON

instance ToJSON ProjectPostCreate where
  toJSON = todoistToJSON

data ProjectPostUpdate = ProjectPostUpdate
  { name :: Maybe Text,
    description :: Maybe Text,
    color :: Maybe Color,
    isFavorite :: Maybe Bool,
    viewStyle :: Maybe ViewStyle
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProjectPostUpdate where
  parseJSON = todoistParseJSON

instance ToJSON ProjectPostUpdate where
  toJSON = todoistToJSON

data Collaborator = Collaborator
  { id :: Text,
    name :: Text,
    email :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Collaborator where
  parseJSON = todoistParseJSON

instance ToJSON Collaborator where
  toJSON = todoistToJSON
