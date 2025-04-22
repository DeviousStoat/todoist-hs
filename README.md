# todoist-hs

Todoist API declaration in servant and API client generated with servant-client

```haskell
import Todoist.Client (projects)
import Todoist.Data.App (mkEnv)

main :: IO ()
main = do
    let apiToken = "..."
    env <- mkEnv apiToken
    p <- runReaderT projects env
    print p
```
