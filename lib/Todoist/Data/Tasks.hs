module Todoist.Data.Tasks where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (toQueryParam))

import Todoist.Data.Common (todoistParseJSON, todoistToJSON)

data Priority
  = Priority1
  | Priority2
  | Priority3
  | Priority4
  deriving (Enum, Show, Eq, Generic)

instance ToJSON Priority where
  toJSON = toJSON . (+ 1) . fromEnum

instance FromJSON Priority where
  parseJSON v = do
    n <- parseJSON v
    if n >= 1 && n <= 4
      then return $ toEnum (n - 1)
      else
        fail $ "Invalid Priority: " ++ show n ++ ". Expected a number between 1 and 4."

data Lang
  = En
  | Da
  | Pl
  | Zh
  | Ko
  | De
  | Pt
  | Ja
  | It
  | Fr
  | Sv
  | Ru
  | Es
  | Nl
  | Fi
  | Nb
  | Tw
  deriving (Show, Eq, Generic)

instance ToJSON Lang where
  toJSON = todoistToJSON

instance FromJSON Lang where
  parseJSON = todoistParseJSON

instance ToHttpApiData Lang where
  toQueryParam = toQueryParam . map toLower . show

data Due = Due
  { string :: Text
  , -- TODO: date
    date :: Text
  , isRecurring :: Bool
  , -- TODO: date
    datetime :: Maybe Text
  , timezone :: Maybe Text
  , lang :: Maybe Lang
  }
  deriving (Show, Eq, Generic)

instance ToJSON Due where
  toJSON = todoistToJSON

instance FromJSON Due where
  parseJSON = todoistParseJSON

data Deadline = Deadline
  -- TODO: date
  { date :: Text
  , lang :: Maybe Lang
  }
  deriving (Show, Eq, Generic)

instance ToJSON Deadline where
  toJSON = todoistToJSON

instance FromJSON Deadline where
  parseJSON = todoistParseJSON

data DurationUnit
  = Minute
  | Day
  deriving (Show, Eq, Generic)

instance ToJSON DurationUnit where
  toJSON = todoistToJSON

instance FromJSON DurationUnit where
  parseJSON = todoistParseJSON

data Duration = Duration
  { amount :: Int
  , unit :: DurationUnit
  }
  deriving (Show, Eq, Generic)

instance ToJSON Duration where
  toJSON = todoistToJSON

instance FromJSON Duration where
  parseJSON = todoistParseJSON

data Task = Task
  { id :: Text
  , assignerId :: Maybe Text
  , assigneeId :: Maybe Text
  , projectId :: Text
  , sectionId :: Maybe Text
  , parentId :: Maybe Text
  , order :: Int
  , content :: Text
  , description :: Text
  , isCompleted :: Bool
  , labels :: [Text]
  , priority :: Priority
  , creatorId :: Text
  , -- TODO: date
    createdAt :: Text
  , due :: Maybe Due
  , url :: Text
  , duration :: Maybe Duration
  , deadline :: Maybe Deadline
  }
  deriving (Show, Eq, Generic)

instance ToJSON Task where
  toJSON = todoistToJSON

instance FromJSON Task where
  parseJSON = todoistParseJSON

data TaskPostCreate = TaskPostCreate
  { content :: Text
  , description :: Maybe Text
  , projectId :: Maybe Text
  , sectionId :: Maybe Text
  , parentId :: Maybe Text
  , order :: Maybe Int
  , labels :: Maybe [Text]
  , priority :: Maybe Priority
  , assigneeId :: Maybe Text
  , dueString :: Maybe Text
  , -- TODO: date
    dueDate :: Maybe Text
  , -- TODO: date
    dueDatetime :: Maybe Text
  , dueLang :: Maybe Lang
  , -- TODO: custom JSON for duration and deadline, check docs
    duration :: Maybe Duration
  , deadline :: Maybe Deadline
  }
  deriving (Eq, Show, Generic)

instance ToJSON TaskPostCreate where
  toJSON = todoistToJSON

instance FromJSON TaskPostCreate where
  parseJSON = todoistParseJSON

data TaskPostUpdate = TaskPostUpdate
  { content :: Maybe Text
  , description :: Maybe Text
  , labels :: Maybe [Text]
  , priority :: Maybe Priority
  , dueString :: Maybe Text
  , -- TODO: date
    dueDate :: Maybe Text
  , -- TODO: date
    dueDatetime :: Maybe Text
  , dueLang :: Maybe Lang
  , assigneeId :: Maybe Text
  , -- TODO: custom JSON for duration and deadline, check docs
    duration :: Maybe Duration
  , deadline :: Maybe Deadline
  }
  deriving (Eq, Show, Generic)

instance ToJSON TaskPostUpdate where
  toJSON = todoistToJSON

instance FromJSON TaskPostUpdate where
  parseJSON = todoistParseJSON
