module Modules.ToDoList.ToDoList where

import Modules.Tasks.Tasks
import Modules.Database.Database

data List = List { user :: String, name :: String, description :: String, tasks :: [Task]}
-- Definindo o tipo de dado que é LIST, quais parâmetros 

createList :: String -> String -> String -> IO()
createList = createListDatabase

getList :: String -> String -> IO()
getList = getListDatabase

addTask :: String -> String -> String -> String -> String -> String -> IO()
addTask = addTaskDatabase