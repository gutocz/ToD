module Modules.Database.Database where
import System.IO
import System.Directory

directoryDatabase :: String
directoryDatabase = "./Modules/Database/LocalUsers/" -- Função que retorna o local padrão dos users criados

createUserDatabase :: String -> String -> String -> String -> IO()
createUserDatabase username name password description = do -- Função que cria um novo usuário no banco de dados
    let user = [username, name, password, description] -- seta 'user' como uma lista que guarda os parâmetros passados
    writeFile (directoryDatabase++username ++ ".txt") (unlines user)
    -- escreve um arquivo txt com os dados da lista anterior, onde o nome do arquivo é o username

deleteUserDatabase :: String -> IO()
deleteUserDatabase username = do -- Função para deletar um user do database
    removeFile (directoryDatabase++username++".txt") -- usa uma função de deletar um arquivo passando o caminho do arquivo

getNameDatabase :: String -> IO String
getNameDatabase username = do -- Função que retorna o nome de um usuário
    conteudo <- readFile (directoryDatabase++username ++ ".txt") -- o termo 'conteudo' recebe os dados lidos no txt
    let linhas = lines conteudo -- seta 'linhas' como uma lista onde cada termo é uma linha de 'conteudo'
    return (linhas !! 1) -- retorna o termo 1 de 'linhas' que é o Nome do user

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