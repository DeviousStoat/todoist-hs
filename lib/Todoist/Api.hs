module Todoist.Api where

import Data.Text (Text)
import Servant.API

import Todoist.Data.Comments (Comment, CommentPostCreate, CommentPostUpdate)
import Todoist.Data.Common (Paginated)
import Todoist.Data.Labels (
  Label,
  LabelPostCreate,
  LabelPostUpdate,
  LabelSharedRemove,
  LabelSharedRename,
 )
import Todoist.Data.Projects (
  Collaborator,
  Project,
  ProjectPostCreate,
  ProjectPostUpdate,
 )
import Todoist.Data.Sections (Section, SectionPostCreate, SectionPostUpdate)
import Todoist.Data.Tasks (
  Lang,
  Task,
  TaskPostCreate,
  TaskPostUpdate,
 )

type ProjectsApi =
  "projects" :> Get '[JSON] (Paginated [Project])
    :<|> "projects" :> ReqBody '[JSON] ProjectPostCreate :> Post '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> Get '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> ReqBody '[JSON] ProjectPostUpdate :> Post '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> "archive" :> Post '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> "unarchive" :> Post '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> Delete '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> "collaborators" :> Get '[JSON] (Paginated [Collaborator])

type SectionsApi =
  "sections" :> QueryParam "project_id" Text :> Get '[JSON] (Paginated [Section])
    :<|> "sections" :> ReqBody '[JSON] SectionPostCreate :> Post '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> Get '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> ReqBody '[JSON] SectionPostUpdate :> Post '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> Delete '[JSON] ()

type TasksApi =
  ( "tasks"
      :> QueryParam "project_id" Text
      :> QueryParam "section_id" Text
      :> QueryParam "label" Text
      :> QueryParam "filter" Text
      :> QueryParam "lang" Lang
      :> QueryParam "ids" Text
      :> Get '[JSON] (Paginated [Task])
  )
    :<|> "tasks" :> ReqBody '[JSON] TaskPostCreate :> Post '[JSON] Task
    :<|> "tasks" :> Capture "task_id" Text :> Get '[JSON] Task
    :<|> "tasks" :> Capture "task_id" Text :> ReqBody '[JSON] TaskPostUpdate :> Post '[JSON] Task
    :<|> "tasks" :> Capture "task_id" Text :> "close" :> Post '[JSON] ()
    :<|> "tasks" :> Capture "task_id" Text :> "reopen" :> Post '[JSON] ()
    :<|> "tasks" :> Capture "task_id" Text :> Delete '[JSON] ()

type CommentsApi =
  "comments" :> QueryParam "project_id" Text :> QueryParam "task_id" Text :> Get '[JSON] (Paginated [Comment])
    :<|> "comments" :> ReqBody '[JSON] CommentPostCreate :> Post '[JSON] Comment
    :<|> "comments" :> Capture "comment_id" Text :> Get '[JSON] Comment
    :<|> "comments" :> Capture "comment_id" Text :> ReqBody '[JSON] CommentPostUpdate :> Post '[JSON] Comment
    :<|> "comments" :> Capture "comment_id" Text :> Delete '[JSON] ()

type LabelsApi =
  "labels" :> Get '[JSON] (Paginated [Label])
    :<|> "labels" :> ReqBody '[JSON] LabelPostCreate :> Post '[JSON] Label
    :<|> "labels" :> Capture "label_id" Text :> Get '[JSON] Label
    :<|> "labels" :> Capture "label_id" Text :> ReqBody '[JSON] LabelPostUpdate :> Post '[JSON] Label
    :<|> "labels" :> Capture "label_id" Text :> Delete '[JSON] ()
    :<|> "labels" :> "shared" :> QueryFlag "omit_personal" :> Get '[JSON] (Paginated [Text])
    :<|> "labels" :> "shared" :> "rename" :> ReqBody '[JSON] LabelSharedRename :> Post '[JSON] ()
    :<|> "labels" :> "shared" :> "remove" :> ReqBody '[JSON] LabelSharedRemove :> Post '[JSON] ()
