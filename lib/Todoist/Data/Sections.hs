module Todoist.Data.Sections where

import Data.Text (Text)

data Section = Section
  { sectionId :: Text
  , sectioProjectId :: Text
  , sectionOrder :: Int
  , sectionName :: Text
  }
  deriving (Show, Eq)

data SectionPostCreate = SectionPostCreate
  { sectionPostCreateName :: Text
  , sectionPostCreateProjectId :: Text
  , sectionPostCreateOrder :: Maybe Int
  }
  deriving (Show, Eq)

newtype SectionPostUpdate = SectionPostUpdate {sectionPostUpdateName :: Text}
  deriving (Show, Eq)
