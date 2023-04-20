import System.IO

module Modules.Users.Users where

data User = User { username :: String, name :: String, password :: String, description :: String }
createUser :: String -> String -> String -> String -> String
createUser username name password description = "bolo"

    