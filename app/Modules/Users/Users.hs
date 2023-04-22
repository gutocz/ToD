module Modules.Users.Users where
import System.IO
import Modules.Database.Database

data User = User { username :: String, name :: String, password :: String, description :: String }

createUser :: String -> String -> String -> String -> IO()
createUser = createUserDatabase

deleteUser :: String -> IO()
deleteUser = deleteUserDatabase

getName :: String -> IO String
getName = getNameDatabase

loginUser :: String -> String -> IO Bool
loginUser = loginDatabase
