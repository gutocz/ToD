module Modules.Tasks.Tasks where

import System.IO
import Modules.Database.Database

data Task = Task { name :: String, description :: String, date :: String, priority :: String }

createTask :: String -> String -> String -> String -> [String]
createTask name description date priority = [name, description, date, priority]