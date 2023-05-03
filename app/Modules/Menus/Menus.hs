module Modules.Menus.Menus where

import System.IO
import System.Exit (exitSuccess)
import Modules.Users.Users
import Modules.Util.ClearScreen
import Modules.ToDoList.ToDoList
import Modules.Database.Database
import Modules.Tasks.Tasks
import Distribution.Compat.Directory (listDirectory)
import Control.Monad
import System.Directory

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

telaListas :: String -> IO ()
telaListas username = do
  clearScreen
  putStrLn "1. Suas Listas\n2. Cadastrar Nova Lista\n3. Sair"
  option <- getLine
  case option of
    "1" -> do
      telaListasPerfil username
      return ()
    "2" -> do
      telaCadastroListas username
      return ()
    "3" -> telaLogin username
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
    createToDoList username listname desc
    _ <- telaListasPerfil username
    return ()


telaAcessoLista :: String -> String -> IO ()
telaAcessoLista username name = do
    clearScreen
    putStrLn "1. Adicionar Tarefa\n2. Listar Tarefas\n3. Sair"
    option <- getLine
    case option of
        "1" -> do
            telaAdicionarTarefa username name
            return ()
        "2" -> do
            telaListarTarefas username name
            return ()
        "3" -> telaListasPerfil username
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaAcessoLista username name

telaListarTarefas :: String -> String -> IO ()
telaListarTarefas username name = do
    clearScreen
    putStrLn "Suas Tarefas: "
    tarefas <- listDirectory (directoryDatabase ++ username ++ "\\listas\\" ++ name ++ "\\")
    putStrLn "0. Voltar"
    let tarefas' = zip [1..] tarefas
    forM_ tarefas' $ \(i, tarefa) -> do
        putStrLn $ show i ++ ". " ++ tarefa
    option <- getLine
    case option of
        "0" -> telaAcessoLista username name
        _ -> do
            let tarefa = tarefas' !! (read option - 1)
            telaAcessoTarefa username name (snd tarefa)
            return ()

telaAcessoTarefa :: String -> String -> String -> IO ()
telaAcessoTarefa username name task = do
    clearScreen
    --putStrLn "1. Editar Tarefa\n2. Excluir Tarefa\n3. Sair"
    --option <- getLine
    --case option of
    --    "1" -> do
    --        telaEditarTarefa username name task
    --        return ()
    --    "2" -> do
    --        telaExcluirTarefa username name task
    --        return ()
    --    "3" -> telaListarTarefas username name
    --    _ -> do
    --        putStrLn "Opção inválida, tente novamente."
    --        telaAcessoTarefa username name task

telaAdicionarTarefa :: String -> String -> IO()
telaAdicionarTarefa username namelist= do
    clearScreen
    putStrLn "Nome da Tarefa: "
    taskname <- getLine
    putStrLn "Descreva sua Tarefa: "
    desc <- getLine
    putStrLn "Pra qual dia você quer adicionar essa tarefa? (dd/mm/aaaa)"
    date <- getLine
    putStrLn "Qual a prioridade dessa tarefa? (1 - 5)"
    priority <- getLine
    addTask username namelist taskname desc date priority
    telaListarTarefas username namelist

telaListasPerfil :: String -> IO ()
telaListasPerfil username = do
    clearScreen
    putStrLn "Suas Listas: "
    listas <- listDirectory (directoryDatabase ++ username ++ "\\listas\\")
    putStrLn "0. Voltar"
    let listas' = zip [1..] listas
    forM_ listas' $ \(i, lista) -> do
        putStrLn $ show i ++ ". " ++ lista
    option <- getLine
    case option of
        "0" -> telaLogin username
        _ -> do
            let list = listas' !! (read option - 1)
            telaAcessoLista username (snd list)
            return ()