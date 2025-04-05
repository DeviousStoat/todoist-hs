module Todoist.Data.Projects where

import Data.Text (Text)

import Todoist.Data.Common (Color, ViewStyle)

data Project = Project
  { projectId :: Text
  , projectName :: Text
  , projectColor :: Color
  , projectParentId :: Maybe Text
  , projectOrder :: Int
  , projectCommentCount :: Int
  , projectIsShared :: Bool
  , projectIsFavorite :: Bool
  , projectIsInboxProject :: Bool
  , projectIsTeamInbox :: Bool
  , projectViewStyle :: ViewStyle
  , projectUrl :: Text
  }
  deriving (Show, Eq)

data ProjectPostCreate = ProjectPostCreate
  { projectPostCreateName :: Text
  , projectPostCreateParentId :: Maybe Text
  , projectPostCreateColor :: Maybe Color
  , projectPostCreateIsFavorite :: Maybe Bool
  , projectPostCreateViewStyle :: Maybe ViewStyle
  }
  deriving (Show, Eq)

data ProjectPostUpdate = ProjectPostUpdate
  { projectPostUpdateName :: Maybe Text
  , projectPostUpdateColor :: Maybe Color
  , projectPostUpdateIsFavorite :: Maybe Bool
  , projectPostUpdateViewStyle :: Maybe ViewStyle
  }
  deriving (Show, Eq)

data Collaborator = Collaborator
  { collaboratorId :: Text
  , collaboratorName :: Text
  , collaboratorEmail :: Text
  }
  deriving (Show, Eq)
