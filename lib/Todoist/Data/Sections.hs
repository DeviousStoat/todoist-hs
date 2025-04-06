module Todoist.Data.Sections where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

import Todoist.Data.Common (todoistParseJSON, todoistToJSON)

data Section = Section
  { id :: Text
  , projectId :: Text
  , order :: Int
  , name :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Section where
  parseJSON = todoistParseJSON

instance ToJSON Section where
  toJSON = todoistToJSON

data SectionPostCreate = SectionPostCreate
  { name :: Text
  , projectId :: Text
  , order :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON SectionPostCreate where
  parseJSON = todoistParseJSON

instance ToJSON SectionPostCreate where
  toJSON = todoistToJSON

newtype SectionPostUpdate = SectionPostUpdate {name :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON SectionPostUpdate where
  parseJSON = todoistParseJSON

instance ToJSON SectionPostUpdate where
  toJSON = todoistToJSON
