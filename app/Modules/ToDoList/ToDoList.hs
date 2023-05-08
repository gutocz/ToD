module Modules.ToDoList.ToDoList where

import Modules.Database.Database

data ToDoList = ToDoList { listName :: String, listDescription :: String }

createToDoList :: String -> String -> String -> IO()
createToDoList = createToDoListDatabase

addTask :: String -> String -> String -> String -> String -> String -> IO()
addTask = addTaskDatabase

deleteTask :: String -> String -> String -> IO()
deleteTask = deleteTaskDatabase

editTask :: String -> String -> String -> String -> String -> IO()
editTask = editTaskDatabase