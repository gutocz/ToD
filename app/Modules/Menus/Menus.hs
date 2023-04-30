module Modules.Menus.Menus where

import System.IO
import System.Exit (exitSuccess)
import Modules.Users.Users
import Modules.Util.ClearScreen
import Modules.ToDoList.ToDoList
import Modules.Database.Database
import Modules.Tasks.Tasks

menuInicial :: IO()
menuInicial = do
    clearScreen
    putStrLn "SALVE\nMENU FODA"
    putStrLn "Selecione uma opção:\n1. Cadastrar novo usuário\n2. Fazer Login\n3. Sair"
    a <- getLine
    let option = read a :: Int
    case option of
        1 -> cadastro
        2 -> login
        3 -> exitSuccess
        _ -> putStrLn "Erro"

cadastro :: IO()
cadastro = do
    clearScreen
    putStrLn "Digite seu username: "
    name <- getLine
    putStrLn "Digite seu nome: "
    username <- getLine
    putStrLn "Digite sua senha: "
    password <- getLine
    putStrLn "Digite sua descrição: "
    desc <- getLine
    createUser name username password desc
    menuInicial

login :: IO()
login = do
    clearScreen
    putStrLn "Username: "
    username <- getLine
    putStrLn "Senha: "
    password <- getLine
    statusLogin <- loginUser username password
    if statusLogin then do
        telaLogin username
    else do
        putStrLn "Dados Incorretos"
        putStrLn "Tentar Novamente?"
        putStrLn "Sim (S) - Não (N)"
        resp <- getLine
        case resp of
            "S" -> login
            "N" -> menuInicial
            "s" -> login
            "n" -> menuInicial
            _   -> putStrLn "Opção Inválida"

telaLogin :: String -> IO()
telaLogin username = do
    clearScreen
    putStrLn "1. Perfil\n2. Listas\n3. Sair"
    option <- getLine
    case (read option :: Int) of
        1 -> putStrLn "telaPerfil"
        2 -> telaListas username
        3 -> menuInicial
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaLogin username

telaListas :: String -> IO()
telaListas username = do
    clearScreen
    putStrLn "1. Suas Listas\n2. Cadastrar Nova Lista\n3. Sair"
    option <- getLine
    case (read option :: Int) of
        1 -> telaListasPerfil username
        2 -> telaCadastroListas username
        3 -> telaLogin username
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaListas username

telaCadastroListas :: String -> IO ()
telaCadastroListas username = do
    clearScreen
    putStrLn "Nome da Lista: "
    listname <- getLine
    putStrLn "Descreva sua Lista: "
    desc <- getLine
    let newList = ToDoList listname desc []
    let filePath = directoryDatabase ++ username ++ "/" ++ username ++ "/" ++ "listas" ++ listname ++ ".txt"
    saveToDoList newList filePath
    telaAcessoLista username listname

telaAcessoLista :: String -> String -> IO ()
telaAcessoLista username name = do
    clearScreen

telaAdicionarTarefa :: String -> String -> IO()
telaAdicionarTarefa username namelist= do
    clearScreen

telaListasPerfil :: String -> IO()
telaListasPerfil username = do
    clearScreen