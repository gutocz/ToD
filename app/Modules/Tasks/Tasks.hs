module Modules.Tasks.Tasks where

import System.IO

data Task = Task { name :: String, description :: String, date :: String, priority :: String }

createTask :: String -> String -> String -> String -> Task
createTask name desc date priority = Task { name = name, description = desc, date = date, priority = priority }

instance Show Task where
    show (Task name desc date priority) = "Task { name = \"" ++ name ++ "\", description = \"" ++ desc ++ "\", date = \"" ++ date ++ "\", priority = \"" ++ priority ++ "\" }"

instance Read Task where
    readsPrec _ str =
        let (name, rest1) = break (== ',') str
            (desc, rest2) = break (== ',') (drop 1 rest1)
            (date, rest3) = break (== ',') (drop 1 rest2)
            (priority, _) = break (== ',') (drop 1 rest3)
        in [(Task name desc date priority, "")]
