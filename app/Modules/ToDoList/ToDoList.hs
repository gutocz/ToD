module Modules.ToDoList.ToDoList where

import Modules.Database.Database

data ToDoList = ToDoList { listName :: String, listDescription :: String }

createToDoList :: String -> String -> String -> IO()
createToDoList = createToDoListDatabase

deleteToDoList :: String -> String -> IO()
deleteToDoList = deleteToDoListDatabase

addUserToList :: String -> String -> String -> IO()
addUserToList = addUserToListDatabase

removeUserFromList :: String -> String -> IO ()
removeUserFromList = removeUserFromListDatabase

getSharedList :: String -> IO [String]
getSharedList = getSharedListDatabase

addTask :: String -> String -> String -> String -> String -> String -> IO()
addTask = addTaskDatabase

showTaskContent :: String -> String -> String -> IO [String]
showTaskContent = showTaskContentDatabase

deleteTask :: String -> String -> String -> IO()
deleteTask = deleteTaskDatabase

editTask :: String -> String -> String -> String -> String -> IO()
editTask = editTaskDatabase