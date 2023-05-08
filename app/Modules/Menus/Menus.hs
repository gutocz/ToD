module Modules.Menus.Menus where

import System.IO
import System.Exit (exitSuccess)
import Modules.Users.Users
import Modules.Util.ClearScreen
import Modules.ToDoList.ToDoList
import Modules.Database.Database
import Distribution.Compat.Directory (listDirectory)
import Control.Monad
import System.Directory
import System.FilePath

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
        1 -> telaPerfil username
        2 -> telaListas username
        3 -> menuInicial
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaLogin username

telaPerfil :: String -> IO()
telaPerfil username = do
    clearScreen
    putStrLn "1. Editar Perfil\n2. Sair"
    option <- getLine
    case option of
        "1" -> do
            telaEditarUsuario username
        "2" -> telaLogin username
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaPerfil username

telaEditarUsuario :: String -> IO()
telaEditarUsuario username = do
    clearScreen
    putStrLn "1. Nome\n2. Username\n3. Senha\n4. Descrição\n5. Sair"
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Novo Nome: "
            name <- getLine
            editUser username name "" ""
            telaPerfil username
        "2" -> do
            putStrLn "Novo Username: "
            username <- getLine
            editUser username "" "" ""
            telaPerfil username
        "3" -> do
            putStrLn "Nova Senha: "
            password <- getLine
            editUser username "" password ""
            telaPerfil username
        "4" -> do
            putStrLn "Nova Descrição: "
            desc <- getLine
            editUser username "" "" desc
            telaPerfil username
        "5" -> telaLogin username
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaEditarUsuario username

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
        let path = directoryDatabase ++ username ++ "\\listas\\" ++ name ++ "\\" ++ tarefa
        isFile <- doesFileExist path
        if isFile && takeExtension tarefa /= ".txt"
          then putStrLn $ show (i) ++ ". " ++ tarefa
          else return ()
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
    putStrLn "1. Editar Tarefa\n2. Excluir Tarefa\n3. Sair"
    option <- getLine
    case option of
        "1" -> do
            telaEditarTarefa username name task
            return ()
        "2" -> do
            telaExcluirTarefa username name task
            return ()
        "3" -> telaListarTarefas username name
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaAcessoTarefa username name task

telaExcluirTarefa :: String -> String -> String -> IO ()
telaExcluirTarefa username name task = do
    clearScreen
    putStrLn "Tem certeza que deseja excluir essa tarefa?"
    putStrLn "Sim (S) - Não (N)"
    resp <- getLine
    case resp of
        "S" -> do
            deleteTask username name task
            telaListarTarefas username name
        "N" -> telaAcessoTarefa username name task
        "s" -> do
            deleteTask username name task
            telaListarTarefas username name
        "n" -> telaAcessoTarefa username name task
        _   -> do
            putStrLn "Opção Inválida"
            telaExcluirTarefa username name task

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

telaEditarTarefa :: String -> String -> String -> IO ()
telaEditarTarefa username namelist task = do
    clearScreen
    putStrLn "1. Nome da Tarefa\n2. Descrição da Tarefa\n3. Data da Tarefa\n4. Prioridade da Tarefa\n5. Sair"
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Novo nome da Tarefa: "
            newname <- getLine
            editTask username namelist task newname "name"
            telaAcessoTarefa username namelist task
        "2" -> do
            putStrLn "Nova descrição da Tarefa: "
            newdesc <- getLine
            editTask username namelist task newdesc "desc"
            telaAcessoTarefa username namelist task
        "3" -> do
            putStrLn "Nova data da Tarefa: "
            newdate <- getLine
            editTask username namelist task newdate "date"
            telaAcessoTarefa username namelist task
        "4" -> do
            putStrLn "Nova prioridade da Tarefa: "
            newpriority <- getLine
            editTask username namelist task newpriority "priority"
            ifNewTaskExists username namelist task
            telaAcessoTarefa username namelist task
        "5" -> telaAcessoTarefa username namelist task
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaEditarTarefa username namelist task

telaListasPerfil :: String -> IO ()
telaListasPerfil username = do
    clearScreen
    putStrLn "Suas Listas: "
    listas <- listDirectory (directoryDatabase ++ username ++ "\\listas\\")
    putStrLn "0. Voltar"
    let listas' = zip [1..] $ filter (not . (==".txt") . takeExtension) listas
    forM_ listas' $ \(i, lista) -> do
        putStrLn $ show i ++ ". " ++ lista
    option <- getLine
    case option of
        "0" -> telaListas username
        _ -> do
            let list = listas' !! (read option - 1)
            telaAcessoLista username (snd list)
            return ()