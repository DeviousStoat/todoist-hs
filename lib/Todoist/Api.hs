module Todoist.Api where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API
import Servant.Client (ClientM, client)

import Todoist.Data.Comments (Comment, CommentPostCreate, CommentPostUpdate)
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
  "projects" :> Get '[JSON] [Project]
    :<|> "projects" :> ReqBody '[JSON] ProjectPostCreate :> Post '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> Get '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> ReqBody '[JSON] ProjectPostUpdate :> Post '[JSON] Project
    :<|> "projects" :> Capture "project_id" Text :> "archive" :> Post '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> "unarchive" :> Post '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> Delete '[JSON] ()
    :<|> "projects" :> Capture "project_id" Text :> "collaborators" :> Get '[JSON] [Collaborator]

projects :: ClientM [Project]
createProject :: ProjectPostCreate -> ClientM Project
project :: Text -> ClientM Project
updateProject :: Text -> ProjectPostUpdate -> ClientM Project
archiveProject :: Text -> ClientM ()
unarchiveProject :: Text -> ClientM ()
deleteProject :: Text -> ClientM ()
projectCollaborators :: Text -> ClientM [Collaborator]
projects
  :<|> createProject
  :<|> project
  :<|> updateProject
  :<|> archiveProject
  :<|> unarchiveProject
  :<|> deleteProject
  :<|> projectCollaborators = client (Proxy :: Proxy ProjectsApi)

type SectionsApi =
  "sections" :> QueryParam "project_id" Text :> Get '[JSON] [Section]
    :<|> "sections" :> ReqBody '[JSON] SectionPostCreate :> Post '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> Get '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> ReqBody '[JSON] SectionPostUpdate :> Post '[JSON] Section
    :<|> "sections" :> Capture "section_id" Text :> Delete '[JSON] ()

sections :: Maybe Text -> ClientM [Section]
createSection :: SectionPostCreate -> ClientM Section
section :: Text -> ClientM Section
updateSection :: Text -> SectionPostUpdate -> ClientM Section
deleteSection :: Text -> ClientM ()
sections
  :<|> createSection
  :<|> section
  :<|> updateSection
  :<|> deleteSection = client (Proxy :: Proxy SectionsApi)

type TasksApi =
  ( "tasks"
      :> QueryParam "project_id" Text
      :> QueryParam "section_id" Text
      :> QueryParam "label" Text
      :> QueryParam "filter" Text
      :> QueryParam "lang" Lang
      :> QueryParam "ids" Text
      :> Get '[JSON] [Task]
  )
    :<|> "tasks" :> ReqBody '[JSON] TaskPostCreate :> Post '[JSON] Task
    :<|> "tasks" :> Capture "task_id" Text :> Get '[JSON] Task
    :<|> "tasks" :> Capture "task_id" Text :> ReqBody '[JSON] TaskPostUpdate :> Post '[JSON] Task
    :<|> "tasks" :> Capture "task_id" Text :> "close" :> Post '[JSON] ()
    :<|> "tasks" :> Capture "task_id" Text :> "reopen" :> Post '[JSON] ()
    :<|> "tasks" :> Capture "task_id" Text :> Delete '[JSON] ()

tasks ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Lang ->
  Maybe Text ->
  ClientM [Task]
createTask :: TaskPostCreate -> ClientM Task
task :: Text -> ClientM Task
updateTask :: Text -> TaskPostUpdate -> ClientM Task
closeTask :: Text -> ClientM ()
reopenTask :: Text -> ClientM ()
deleteTask :: Text -> ClientM ()
tasks
  :<|> createTask
  :<|> task
  :<|> updateTask
  :<|> closeTask
  :<|> reopenTask
  :<|> deleteTask = client (Proxy :: Proxy TasksApi)

type CommentsApi =
  "comments" :> QueryParam "project_id" Text :> QueryParam "task_id" Text :> Get '[JSON] [Comment]
    :<|> "comments" :> ReqBody '[JSON] CommentPostCreate :> Post '[JSON] Comment
    :<|> "comments" :> Capture "comment_id" Text :> Get '[JSON] Comment
    :<|> "comments" :> Capture "comment_id" Text :> ReqBody '[JSON] CommentPostUpdate :> Post '[JSON] Comment
    :<|> "comments" :> Capture "comment_id" Text :> Delete '[JSON] ()

comments :: Maybe Text -> Maybe Text -> ClientM [Comment]
createComment :: CommentPostCreate -> ClientM Comment
comment :: Text -> ClientM Comment
updateComment :: Text -> CommentPostUpdate -> ClientM Comment
deleteComment :: Text -> ClientM ()
comments
  :<|> createComment
  :<|> comment
  :<|> updateComment
  :<|> deleteComment = client (Proxy :: Proxy CommentsApi)

type LabelsApi =
  "labels" :> Get '[JSON] [Label]
    :<|> "labels" :> ReqBody '[JSON] LabelPostCreate :> Post '[JSON] Label
    :<|> "labels" :> Capture "label_id" Text :> Get '[JSON] Label
    :<|> "labels" :> Capture "label_id" Text :> ReqBody '[JSON] LabelPostUpdate :> Post '[JSON] Label
    :<|> "labels" :> Capture "label_id" Text :> Delete '[JSON] ()
    :<|> "labels" :> "shared" :> QueryFlag "omit_personal" :> Get '[JSON] [Text]
    :<|> "labels" :> "shared" :> "rename" :> ReqBody '[JSON] LabelSharedRename :> Post '[JSON] ()
    :<|> "labels" :> "shared" :> "remove" :> ReqBody '[JSON] LabelSharedRemove :> Post '[JSON] ()

labels :: ClientM [Label]
createLabel :: LabelPostCreate -> ClientM Label
label :: Text -> ClientM Label
updateLabel :: Text -> LabelPostUpdate -> ClientM Label
deleteLabel :: Text -> ClientM ()
sharedLabels :: Bool -> ClientM [Text]
renameSharedLabel :: LabelSharedRename -> ClientM ()
removeSharedLabel :: LabelSharedRemove -> ClientM ()
labels
  :<|> createLabel
  :<|> label
  :<|> updateLabel
  :<|> deleteLabel
  :<|> sharedLabels
  :<|> renameSharedLabel
  :<|> removeSharedLabel = client (Proxy :: Proxy LabelsApi)
