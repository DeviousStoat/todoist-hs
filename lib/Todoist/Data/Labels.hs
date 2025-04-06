module Todoist.Data.Labels where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

import Todoist.Data.Common (Color, todoistParseJSON, todoistToJSON)

data Label = Label
  { id :: Text
  , name :: Text
  , color :: Color
  , order :: Int
  , isFavorite :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Label where
  toJSON = todoistToJSON

instance FromJSON Label where
  parseJSON = todoistParseJSON

data LabelPostCreate = LabelPostCreate
  { name :: Text
  , order :: Maybe Int
  , color :: Maybe Color
  , isFavorite :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON LabelPostCreate where
  toJSON = todoistToJSON

instance FromJSON LabelPostCreate where
  parseJSON = todoistParseJSON

data LabelPostUpdate = LabelPostUpdate
  { name :: Maybe Text
  , order :: Maybe Int
  , color :: Maybe Color
  , isFavorite :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON LabelPostUpdate where
  toJSON = todoistToJSON

instance FromJSON LabelPostUpdate where
  parseJSON = todoistParseJSON

data LabelSharedRename = LabelSharedRename
  { name :: Text
  , newName :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON LabelSharedRename where
  toJSON = todoistToJSON

instance FromJSON LabelSharedRename where
  parseJSON = todoistParseJSON

newtype LabelSharedRemove = LabelSharedRemove
  {name :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON LabelSharedRemove where
  toJSON = todoistToJSON

instance FromJSON LabelSharedRemove where
  parseJSON = todoistParseJSON
