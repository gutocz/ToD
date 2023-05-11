module Modules.Menus.Menus where

import Control.Monad
import Distribution.Compat.Directory (listDirectory)
import Modules.Database.Database
import Modules.ToDoList.ToDoList
import Modules.Users.Users
import Modules.Util.ClearScreen
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath
import System.IO

menuInicial :: IO ()
menuInicial = do
  clearScreen
  putStrLn ".--------------------------------------------------."
  putStrLn "|oooo     oooo ooooooooooo oooo   oooo ooooo  oooo |"
  putStrLn "| 8888o   888   888    88   8888o  88   888    88  |"
  putStrLn "| 88 888o8 88   888ooo8     88 888o88   888    88  |"
  putStrLn "| 88  888  88   888    oo   88   8888   888    88  |"
  putStrLn "|o88o  8  o88o o888ooo8888 o88o    88    888oo88   |"
  putStrLn "|                                                  |"
  putStrLn "|Selecione uma opção:                              |"
  putStrLn "|                                                  |"
  putStrLn "|1. Cadastrar novo usuário                         |"
  putStrLn "|2. Fazer Login                                    |"
  putStrLn "|3. Sair                                           |"
  putStrLn "|                                                  |"
  putStrLn "'--------------------------------------------------'"
  opcao <- getLine
  case opcao of
    "1" -> cadastro
    "2" -> login
    "3" -> exitSuccess
    _ -> do
      putStrLn "Opção inválida, tente novamente.\n"
      menuInicial

cadastro :: IO ()
cadastro = do
  clearScreen
  putStrLn "Menu>Cadastro"
  putStrLn " .--------------------------------------------------------------------------------------------."
  putStrLn " | oooooooo8     o      ooooooooo      o       oooooooo8 ooooooooooo oooooooooo    ooooooo    |"
  putStrLn " |o888     88    888      888    88o   888     888        88  888  88  888    888 o888   888o |"
  putStrLn " |888           8  88     888    888  8  88     888oooooo     888      888oooo88  888     888 |"
  putStrLn " |888o     oo  8oooo88    888    888 8oooo88           888    888      888  88o   888o   o888 |"
  putStrLn " | 888oooo88 o88o  o888o o888ooo88 o88o  o888o o88oooo888    o888o    o888o  88o8   88ooo88   |"
  putStrLn " |                                                                                            |"
  putStrLn " '--------------------------------------------------------------------------------------------'"
  putStrLn ""
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

login :: IO ()
login = do
  clearScreen
  putStrLn "Menu>Login"
  putStrLn " .---------------------------------------------------------."
  putStrLn " | ooooo         ooooooo     ooooooo8 ooooo oooo   oooo    |"
  putStrLn " |  888        o888   888o o888    88  888   8888o  88     |"
  putStrLn " |  888        888     888 888    oooo 888   88 888o88     |"
  putStrLn " |  888      o 888o   o888 888o    88  888   88   8888     |"
  putStrLn " | o888ooooo88   88ooo88    888ooo888 o888o o88o    88     |"
  putStrLn " |                                                         |"
  putStrLn " '---------------------------------------------------------'"
  putStrLn ""
  putStrLn "Username: "
  username <- getLine
  putStrLn "Senha: "
  password <- getLine
  statusLogin <- loginUser username password
  if statusLogin
    then do
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
        _ -> putStrLn "Opção Inválida"

telaLogin :: String -> IO ()
telaLogin username = do
  clearScreen
  putStrLn "Menu>Login>Opcoes"
  putStrLn ""
  putStrLn "1. Perfil\n2. Listas\n3. Logout"
  option <- getLine
  case (read option :: Int) of
    1 -> telaPerfil username
    2 -> telaListas username
    3 -> menuInicial
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      telaLogin username

telaPerfil :: String -> IO ()
telaPerfil username = do
  clearScreen
  putStrLn "Menu>Login>Opcoes>Perfil>MenuPerfil"
  putStrLn ""
  putStrLn "1. Exibir Perfil\n2. Sair"
  option <- getLine
  case option of
    "1" -> do
      telaExibirPerfil username
    --"2" -> do
    --  telaEditarUsuario username
    "2" -> telaLogin username
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      telaPerfil username

telaExibirPerfil :: String -> IO ()
telaExibirPerfil username = do
  clearScreen
  putStrLn "Menu>Login>Opcoes>Perfil"
  putStrLn "  .------------------------------------------------------------------."
  putStrLn "  |  oooooooooo ooooooooooo oooooooooo  ooooooooooo ooooo ooooo      |"
  putStrLn "  |  888    888 888    88   888    888  888    88   888   888        |"
  putStrLn "  |  888oooo88  888ooo8     888oooo88   888ooo8     888   888        |"
  putStrLn "  |  888        888    oo   888  88o    888         888   888      o |"
  putStrLn "  | o888o      o888ooo8888 o888o  88o8 o888o       o888o o888ooooo88 |"
  putStrLn "  |                                                                  |"
  putStrLn "  '------------------------------------------------------------------'"
  putStrLn ""
  name <- getName username
  putStrLn ("Nome: " ++ name)
  putStrLn ("Username: " ++ username)
  desc <- getDescription username
  putStrLn ("Descrição: " ++ desc)
  putStrLn ""
  putStrLn "0. Sair"
  option <- getLine
  case option of
    "0" -> telaPerfil username
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      telaExibirPerfil username

telaEditarUsuario :: String -> IO ()
telaEditarUsuario username = do
  clearScreen
  putStrLn "Menu>Login>Opcoes>Perfil>EditarPerfil"
  putStrLn ""
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
  putStrLn "Menu>Login>Opcoes>Listas"
  putStrLn ""
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
  putStrLn "Menu>Login>Opcoes>Listas>ListasCompartilhadas"
  putStrLn ""
  putStrLn "Listas Compartilhadas Comigo"
  listas <- getSharedList username
  putStrLn "0. Sair"
  let listas' = zip [1 ..] listas
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
  putStrLn "Menu>Login>Opcoes>Listas>CadastroListas"
  putStrLn ""
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
  putStrLn "Menu>Login>Opcoes>Listas>Tarefas"
  putStrLn ""
  putStrLn "1. Adicionar Tarefa\n2. Listar Tarefas\n3. Compartilhar Lista\n4. Deletar Lista\n5. Sair"
  option <- getLine
  case option of
    "1" -> do
      telaAdicionarTarefa username creator name
      return ()
    "2" -> do
      telaListarTarefas username creator name
      return ()
    "4" -> do
        putStrLn "Tem certeza que deseja deletar a lista? (S/N)"
        option <- getLine
        case option of
            "S" -> do
                deleteToDoList username name
                telaListas username
                return ()
            "N" -> do
                telaAcessoLista username creator name
                return ()
    "5" -> telaListas username
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
  putStrLn "Menu>Login>Opcoes>Listas>Tarefas>ListarTarefas"
  putStrLn ""
  putStrLn "Suas Tarefas: "
  tarefas <- listDirectory (directoryDatabase ++ creator ++ "\\listas\\" ++ name ++ "\\")
  putStrLn "0. Voltar"
  let tarefas' = zip [1 ..] tarefas
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
  putStrLn "Menu>Login>Opcoes>Listas>Tarefas>ListarTarefas>Menu"
  putStrLn ""
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
      -- putStrLn "1. Editar Tarefa"
      -- putStrLn "2. Excluir Tarefa"
      option <- getLine
      case option of
        "0" -> telaAcessoTarefa username creator name task
        -- "1" -> do
        --    telaEditarTarefa username name task
        --    return ()
        -- "2" -> do
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
  putStrLn "Menu>Login>Opcoes>Listas>Tarefas>ListaTarefas>Menu>Excluir"
  putStrLn ""
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
    _ -> do
      putStrLn "Opção Inválida"
      telaExcluirTarefa username creator name task

telaAdicionarTarefa :: String -> String -> String -> IO ()
telaAdicionarTarefa username creator namelist = do
  clearScreen
  putStrLn "Menu>Login>Opcoes>Listas>Tarefas>ListarTarefas>MenuTarefa>AdicionarTarefa"
  putStrLn ""
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
  putStrLn "Menu>Login>Opcoes>Listas>Tarefas>ListarTarefas>MenuTarefa>EditarTarefa"
  putStrLn ""
  putStrLn "Após editar uma tarefa, será necessário reiniciar o programa caso queira re-editar a mesma ou excluí-la."
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
  putStrLn "Menu>Login>Opcoes>Listas>ListasPerfil"
  putStrLn ""
  putStrLn "Suas Listas: "
  listas <- listDirectory (directoryDatabase ++ username ++ "\\listas\\")
  putStrLn "0. Voltar"
  let listas' = zip [1 ..] $ filter (not . (== ".txt") . takeExtension) listas
  forM_ listas' $ \(i, lista) -> do
    putStrLn $ show i ++ ". " ++ lista
  option <- getLine
  case option of
    "0" -> telaListas username
    _ -> do
      let list = listas' !! (read option - 1)
      telaAcessoLista username username (snd list)
      return ()