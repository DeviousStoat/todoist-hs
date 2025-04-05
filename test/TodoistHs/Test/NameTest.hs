module TodoistHs.Test.NameTest where

import Hedgehog (TestT, (===))

import TodoistHs (name)

test_name :: TestT IO ()
test_name = "todoist-hs" === name
