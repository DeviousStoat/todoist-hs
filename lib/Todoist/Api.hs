module Todoist.Api where

import Data.Text (Text)
import Servant.API

import Todoist.Data.Projects (
  Collaborator,
  Project,
  ProjectPostCreate,
  ProjectPostUpdate,
 )
import Todoist.Data.Sections (Section, SectionPostCreate, SectionPostUpdate)

type ProjectsApi =
  "projects" :> Get '[JSON] [Project]
    :<|> "projects" :> ReqBody '[JSON] ProjectPostCreate :> Post '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> Get '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> ReqBody '[JSON] ProjectPostUpdate :> Post '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> "archive" :> Post '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> "unarchive" :> Post '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> Delete '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> "collaborators" :> Get '[JSON] [Collaborator]

type SectionsApi =
  "sections" :> QueryParam "project_id" Text :> Get '[JSON] [Section]
    :<|> "sections" :> ReqBody '[JSON] SectionPostCreate :> Post '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> Get '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> ReqBody '[JSON] SectionPostUpdate :> Post '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> Delete '[JSON] ()
