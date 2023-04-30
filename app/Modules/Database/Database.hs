module Modules.Database.Database where
import System.IO
import System.Directory
import Modules.Tasks.Tasks
import Modules.ToDoList.ToDoList

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
-- Salva uma lista de to do em um arquivo de texto
saveToDoList :: ToDoList -> FilePath -> IO ()
saveToDoList todoList filePath = do
  let listData = listName todoList ++ "\n" ++ listDescription todoList ++ "\n"
                  ++ unlines (map show (tasks todoList))
  withFile filePath WriteMode $ \handle -> do
    hPutStr handle listData
    hClose handle

-- Carrega uma lista de to do de um arquivo de texto
loadToDoList :: FilePath -> IO ToDoList
loadToDoList filePath = do
  withFile filePath ReadMode $ \handle -> do
    listData <- hGetContents handle
    let (name:desc:taskData) = lines listData
        taskList = map read taskData
    hClose handle -- fechar o arquivo aqui
    return $ ToDoList name desc taskList