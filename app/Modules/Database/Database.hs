module Modules.Database.Database where
import System.IO
import System.Directory

directoryDatabase :: String
directoryDatabase = "./Modules/Database/LocalUsers/"

createUserDatabase :: String -> String -> String -> String -> IO()
createUserDatabase username name password description = do
    let user = [username, name, password, description]
    writeFile (directoryDatabase++username ++ ".txt") (unlines user)

deleteUserDatabase :: String -> IO()
deleteUserDatabase username = do
    removeFile (directoryDatabase++username++".txt")

getNameDatabase :: String -> IO String
getNameDatabase username = do
    conteudo <- readFile (directoryDatabase++username ++ ".txt")
    let linhas = lines conteudo
    return (linhas !! 1)

loginDatabase :: String -> String -> IO Bool
loginDatabase username password = do
    let fileName = directoryDatabase ++ username ++ ".txt"
    userExists <- doesFileExist fileName
    if userExists then do
        conteudo <- readFile (directoryDatabase++username ++ ".txt")
        let linhas = lines conteudo
        if linhas !! 2 == password then return True
        else return False
    else return False