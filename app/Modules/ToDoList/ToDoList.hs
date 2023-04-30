module Modules.ToDoList.ToDoList where

import Modules.Tasks.Tasks

data ToDoList = ToDoList { listName :: String, listDescription :: String, tasks :: [Task] }

addTask :: ToDoList -> Task -> ToDoList
addTask (ToDoList name desc tasks) task = ToDoList name desc (task:tasks)

removeTask :: ToDoList -> Int -> ToDoList
removeTask (ToDoList name desc tasks) index = ToDoList name desc (take index tasks ++ drop (index + 1) tasks)

getTask :: ToDoList -> Int -> Task
getTask (ToDoList name desc tasks) index = tasks !! index
