module Modules.Tasks.Tasks where

import System.IO

data Task = Task { name :: String, description :: String, date :: String, priority :: String }

createTask :: String -> String -> String -> String -> [String]
createTask name desc date priority = [name, desc, date, priority]