module Todoist.Client where

import Control.Exception (throw)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API
import Servant.Client
  ( Client,
    ClientM,
    HasClient,
    client,
    hoistClient,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Todoist.Api
  ( CommentsApi,
    LabelsApi,
    ProjectsApi,
    SectionsApi,
    TasksApi,
  )
import Todoist.Data.App (Env (..), TodoistApp)
import Todoist.Data.Comments (Comment, CommentPostCreate, CommentPostUpdate)
import Todoist.Data.Common (Paginated)
import Todoist.Data.Labels
  ( Label,
    LabelPostCreate,
    LabelPostUpdate,
    LabelSharedRemove,
    LabelSharedRename,
  )
import Todoist.Data.Projects
  ( Collaborator,
    Project,
    ProjectPostCreate,
    ProjectPostUpdate,
  )
import Todoist.Data.Sections (Section, SectionPostCreate, SectionPostUpdate)
import Todoist.Data.Tasks
  ( Lang,
    Task,
    TaskPostCreate,
    TaskPostUpdate,
  )

todoistClient :: (HasClient ClientM api) => Proxy api -> Client TodoistApp api
todoistClient api = hoistClient api convertClientM (client api)

convertClientM :: ClientM a -> TodoistApp a
convertClientM clientM = do
  todoistEnv <- ask
  baseUrl <- parseBaseUrl todoistEnv.apiBaseUrl
  let clientEnv = mkClientEnv todoistEnv.httpManager baseUrl
  fmap (either throw id) . lift $ runClientM clientM clientEnv

projects :: TodoistApp (Paginated [Project])
createProject :: ProjectPostCreate -> TodoistApp Project
project :: Text -> TodoistApp Project
updateProject :: Text -> ProjectPostUpdate -> TodoistApp Project
archiveProject :: Text -> TodoistApp ()
unarchiveProject :: Text -> TodoistApp ()
deleteProject :: Text -> TodoistApp ()
projectCollaborators :: Text -> TodoistApp (Paginated [Collaborator])
projects
  :<|> createProject
  :<|> project
  :<|> updateProject
  :<|> archiveProject
  :<|> unarchiveProject
  :<|> deleteProject
  :<|> projectCollaborators = todoistClient (Proxy :: Proxy ProjectsApi)

sections :: Maybe Text -> TodoistApp (Paginated [Section])
createSection :: SectionPostCreate -> TodoistApp Section
section :: Text -> TodoistApp Section
updateSection :: Text -> SectionPostUpdate -> TodoistApp Section
deleteSection :: Text -> TodoistApp ()
sections
  :<|> createSection
  :<|> section
  :<|> updateSection
  :<|> deleteSection = todoistClient (Proxy :: Proxy SectionsApi)

tasks ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Lang ->
  Maybe Text ->
  TodoistApp (Paginated [Task])
createTask :: TaskPostCreate -> TodoistApp Task
task :: Text -> TodoistApp Task
updateTask :: Text -> TaskPostUpdate -> TodoistApp Task
closeTask :: Text -> TodoistApp ()
reopenTask :: Text -> TodoistApp ()
deleteTask :: Text -> TodoistApp ()
tasks
  :<|> createTask
  :<|> task
  :<|> updateTask
  :<|> closeTask
  :<|> reopenTask
  :<|> deleteTask = todoistClient (Proxy :: Proxy TasksApi)

comments :: Maybe Text -> Maybe Text -> TodoistApp (Paginated [Comment])
createComment :: CommentPostCreate -> TodoistApp Comment
comment :: Text -> TodoistApp Comment
updateComment :: Text -> CommentPostUpdate -> TodoistApp Comment
deleteComment :: Text -> TodoistApp ()
comments
  :<|> createComment
  :<|> comment
  :<|> updateComment
  :<|> deleteComment = todoistClient (Proxy :: Proxy CommentsApi)

labels :: TodoistApp (Paginated [Label])
createLabel :: LabelPostCreate -> TodoistApp Label
label :: Text -> TodoistApp Label
updateLabel :: Text -> LabelPostUpdate -> TodoistApp Label
deleteLabel :: Text -> TodoistApp ()
sharedLabels :: Bool -> TodoistApp (Paginated [Text])
renameSharedLabel :: LabelSharedRename -> TodoistApp ()
removeSharedLabel :: LabelSharedRemove -> TodoistApp ()
labels
  :<|> createLabel
  :<|> label
  :<|> updateLabel
  :<|> deleteLabel
  :<|> sharedLabels
  :<|> renameSharedLabel
  :<|> removeSharedLabel = todoistClient (Proxy :: Proxy LabelsApi)
