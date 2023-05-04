{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Modules.Database.Database where
import System.IO
import System.Directory
import Control.Monad (filterM)

directoryDatabase :: String
directoryDatabase = "./Modules/Database/LocalUsers/" -- Função que retorna o local padrão dos users criados

--Funções relacionadas a users
createUserDatabase :: String -> String -> String -> String -> IO()
createUserDatabase username name password description = do -- Função que cria um novo usuário no banco de dados
    let user = [username, name, password, description] -- seta 'user' como uma lista que guarda os parâmetros passados
    createDirectory (directoryDatabase ++ username)
    createDirectory (directoryDatabase ++ "/" ++ username ++ "/" ++ "listas")
    writeFile (directoryDatabase++username++"/"++username++ ".txt") (unlines user)
    -- escreve um arquivo txt com os dados da lista anterior, onde o nome do arquivo é o username

deleteUserDatabase :: String -> IO()
deleteUserDatabase username = do -- Função para deletar um user do database
    removeFile (directoryDatabase++username++"/"++username++".txt") -- usa uma função de deletar um arquivo passando o caminho do arquivo

getNameDatabase :: String -> IO String
getNameDatabase username = do -- Função que retorna o nome de um usuário
    conteudo <- readFile (directoryDatabase++username++"/"++username ++ ".txt") -- o termo 'conteudo' recebe os dados lidos no txt
    let linhas = lines conteudo -- seta 'linhas' como uma lista onde cada termo é uma linha de 'conteudo'
    return (linhas !! 1) -- retorna o termo 1 de 'linhas' que é o Nome do user

--funções relacionadas ao login
loginDatabase :: String -> String -> IO Bool
loginDatabase username password = do
    let fileName = directoryDatabase ++ username++"/"++username ++ ".txt"
    userExists <- doesFileExist fileName
    if userExists then do
        conteudo <- readFile (directoryDatabase++username++"/"++username ++ ".txt")
        let linhas = lines conteudo
        if linhas !! 2 == password then return True
        else return False
    else return False

--funções relacionadas a listas
createToDoListDatabase :: String -> String -> String -> IO()
createToDoListDatabase username listName listdesc = do
    let listcontent = [listName, listdesc]
    existFile <- doesDirectoryExist (directoryDatabase ++ username ++ "/listas"++"/"++listName)
    if not existFile then do
        createDirectory (directoryDatabase ++ username ++ "/listas"++"/"++listName)
        writeFile (directoryDatabase++username++"/listas/"++listName++"/"++listName++ ".txt") (unlines listcontent)
    else return ()

addTaskDatabase :: String -> String -> String -> String -> String -> String -> IO()
addTaskDatabase username listName taskName taskDesc taskDate taskPriority = do
    let taskcontent = [taskName, taskDesc, taskDate, taskPriority]
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"
    writeFile (filePath ++ taskName) (unlines taskcontent)

deleteTaskDatabase :: String -> String -> String -> IO()
deleteTaskDatabase username listName taskName = do
    let filePath = directoryDatabase++username++"/listas/"++listName++"/"
    removeFile (filePath ++ taskName)

editTaskDatabase :: String -> String -> String -> String -> String -> IO()
editTaskDatabase username listName taskName newData oldData = do
    putStrLn "bolo"