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
    putStrLn "========== MENU INICIAL =========="
    putStrLn "Selecione uma opção:\n"
    putStrLn "1. Cadastrar novo usuário"
    putStrLn "2. Fazer Login"
    putStrLn "3. Sair"
    putStrLn "=================================="
    opcao <- getLine
    case opcao of
        "1" -> cadastro
        "2" -> login
        "3" -> exitSuccess
        _   -> do
            putStrLn "Opção inválida, tente novamente.\n"
            menuInicial

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
    putStrLn "1. Perfil\n2. Listas\n3. Logout"
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
    putStrLn "1. Exibir Perfil\n2. Editar Perfil\n3. Sair"
    option <- getLine
    case option of
        "1" -> do
            telaExibirPerfil username
        "2" -> do
            telaEditarUsuario username
        "3" -> telaLogin username
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaPerfil username

telaExibirPerfil :: String -> IO ()
telaExibirPerfil username = do
    clearScreen
    putStrLn "=================================="
    putStrLn "            PERFIL"
    putStrLn "=================================="
    putStrLn ""
    name <- getName username
    putStrLn ("Nome: " ++ name)
    putStrLn ("Username: " ++ username)
    desc <- getDescription username
    putStrLn ("Descrição: " ++ desc)
    putStrLn ""
    putStrLn "0. Sair"
    putStrLn "=================================="
    option <- getLine
    case option of
        "0" -> telaPerfil username
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            telaExibirPerfil username



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
  putStrLn "1. Suas Listas\n2. Listas Compartilhadas Comigo\n3. Cadastrar Nova Lista\n4. Sair"
  option <- getLine
  case option of
    "1" -> do
      telaListasPerfil username
      return ()
    "3" -> do
      telaCadastroListas username
      return ()
    "4" -> telaLogin username
    "2" -> do
      telaListasCompartilhadas username
      return ()
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      telaListas username

telaListasCompartilhadas :: String -> IO ()
telaListasCompartilhadas username = do
    clearScreen
    putStrLn "Listas Compartilhadas Comigo"
    listas <- getSharedList username
    putStrLn "0. Sair"
    let listas' = zip [1..] listas
    forM_ listas' $ \(i, lista) -> do
        putStrLn $ show i ++ ". " ++ lista
    option <- getLine
    case option of
        "0" -> telaListas username
        _ -> do
            let listname = listas !! (read option - 1)
            finallist <- readFile (directoryDatabase ++ username ++ "/sharedWithMe/" ++ listname)
            let creator = head $ lines finallist
            telaAcessoLista username creator listname


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


telaAcessoLista :: String -> String -> String -> IO ()
telaAcessoLista username creator name = do
    clearScreen
    putStrLn "1. Adicionar Tarefa\n2. Listar Tarefas\n3. Compartilhar Lista\n4. Sair"
    option <- getLine
    case option of
        "1" -> do
            telaAdicionarTarefa username creator name
            return ()
        "2" -> do
            telaListarTarefas username creator name
            return ()
        "4" -> telaListas username
        "3" -> do
            putStrLn "Digite o username do usuário que deseja compartilhar a lista: "
            username' <- getLine
            addUserToList username' creator name
            telaAcessoLista username creator name
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaAcessoLista username creator name

telaListarTarefas :: String -> String -> String -> IO ()
telaListarTarefas username creator name = do
    clearScreen
    putStrLn "Suas Tarefas: "
    tarefas <- listDirectory (directoryDatabase ++ creator ++ "\\listas\\" ++ name ++ "\\")
    putStrLn "0. Voltar"
    let tarefas' = zip [1..] tarefas
    forM_ tarefas' $ \(i, tarefa) -> do
        let path = directoryDatabase ++ creator ++ "\\listas\\" ++ name ++ "\\" ++ tarefa
        isFile <- doesFileExist path
        if isFile && takeExtension tarefa /= ".txt"
          then putStrLn $ show (i) ++ ". " ++ tarefa
          else return ()
    option <- getLine
    case option of
        "0" -> telaAcessoLista username creator name
        _ -> do
            let tarefa = tarefas' !! (read option - 1)
            telaAcessoTarefa username creator name (snd tarefa)
            return ()

telaAcessoTarefa :: String -> String -> String -> String -> IO ()
telaAcessoTarefa username creator name task = do
    clearScreen
    putStrLn "1. Exibir Tarefa\n2. Editar Tarefa\n3. Excluir Tarefa\n4. Sair"
    option <- getLine
    case option of
        "2" -> do
            telaEditarTarefa username creator name task
            return ()
        "3" -> do
            telaExcluirTarefa username creator name task
            return ()
        "4" -> telaListarTarefas username creator name
        "1" -> do
            clearScreen
            linhas <- showTaskContent creator name task
            putStrLn $ "Nome: " ++ (linhas !! 0)
            putStrLn $ "Descrição: " ++ (linhas !! 1)
            putStrLn $ "Data: " ++ (linhas !! 2)
            putStrLn $ "Prioridade: " ++ (linhas !! 3)
            putStrLn ""
            putStrLn "0. Voltar"
            --putStrLn "1. Editar Tarefa"
            --putStrLn "2. Excluir Tarefa"
            option <- getLine
            case option of
                "0" -> telaAcessoTarefa username creator name task
                --"1" -> do
                --    telaEditarTarefa username name task
                --    return ()
                --"2" -> do
                --    telaExcluirTarefa username name task
                --    return ()
                _ -> do
                    putStrLn "Opção inválida, tente novamente."
                    telaAcessoTarefa username creator name task
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaAcessoTarefa username creator name task

telaExcluirTarefa :: String -> String -> String -> String -> IO ()
telaExcluirTarefa username creator name task = do
    clearScreen
    putStrLn "Tem certeza que deseja excluir essa tarefa?"
    putStrLn "Sim (S) - Não (N)"
    resp <- getLine
    case resp of
        "S" -> do
            deleteTask creator name task
            telaListarTarefas username creator name
        "N" -> telaAcessoTarefa username creator name task
        "s" -> do
            deleteTask creator name task
            telaListarTarefas username creator name
        "n" -> telaAcessoTarefa username creator name task
        _   -> do
            putStrLn "Opção Inválida"
            telaExcluirTarefa username creator name task

telaAdicionarTarefa :: String -> String -> String -> IO()
telaAdicionarTarefa username creator namelist= do
    clearScreen
    putStrLn "Nome da Tarefa: "
    taskname <- getLine
    putStrLn "Descreva sua Tarefa: "
    desc <- getLine
    putStrLn "Pra qual dia você quer adicionar essa tarefa? (dd/mm/aaaa)"
    date <- getLine
    putStrLn "Qual a prioridade dessa tarefa? (1 - 5)"
    priority <- getLine
    addTask creator namelist taskname desc date priority
    telaListarTarefas username creator namelist

telaEditarTarefa :: String -> String -> String -> String -> IO ()
telaEditarTarefa username creator namelist task = do
    clearScreen
    putStrLn "1. Nome da Tarefa\n2. Descrição da Tarefa\n3. Data da Tarefa\n4. Prioridade da Tarefa\n5. Sair"
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Novo nome da Tarefa: "
            newname <- getLine
            editTask username namelist task newname "name"
            telaAcessoTarefa username creator namelist task
        "2" -> do
            putStrLn "Nova descrição da Tarefa: "
            newdesc <- getLine
            editTask username namelist task newdesc "desc"
            telaAcessoTarefa username creator namelist task
        "3" -> do
            putStrLn "Nova data da Tarefa: "
            newdate <- getLine
            editTask username namelist task newdate "date"
            telaAcessoTarefa username creator namelist task
        "4" -> do
            putStrLn "Nova prioridade da Tarefa: "
            newpriority <- getLine
            editTask username namelist task newpriority "priority"
            telaAcessoTarefa username creator namelist task
        "5" -> telaAcessoTarefa username creator namelist task
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaEditarTarefa username creator namelist task

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
            telaAcessoLista username username (snd list)
            return ()