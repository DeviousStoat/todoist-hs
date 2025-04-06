module Todoist.Data.Comments where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

import Todoist.Data.Common (todoistParseJSON, todoistToJSON)

data Comment = Comment
  { id :: Text
  , taskId :: Maybe Text
  , projectId :: Maybe Text
  , -- TODO: date
    postedAt :: Text
  , content :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Comment where
  toJSON = todoistToJSON

instance FromJSON Comment where
  parseJSON = todoistParseJSON

data CommentPostCreate = CommentPostCreate
  { taskId :: Maybe Text
  , projectId :: Maybe Text
  , content :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CommentPostCreate where
  toJSON = todoistToJSON

instance FromJSON CommentPostCreate where
  parseJSON = todoistParseJSON

newtype CommentPostUpdate = CommentPostUpdate
  {content :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON CommentPostUpdate where
  toJSON = todoistToJSON

instance FromJSON CommentPostUpdate where
  parseJSON = todoistParseJSON
