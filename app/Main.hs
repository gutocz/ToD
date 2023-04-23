import System.IO
import System.Exit (exitSuccess)
import Modules.Users.Users
import Modules.Util.ClearScreen

main :: IO ()
main = do
    menuInicial

menuInicial :: IO()
menuInicial = do
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
        telaLogin
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


telaLogin :: IO()
telaLogin = do
    clearScreen
    putStrLn "1. Perfil\n2. Listas\n3. Sair"
    option <- getLine
    case (read option :: Int) of
        1 -> putStrLn "telaPerfil"
        2 -> putStrLn "Listas"
        3 -> menuInicial
        _ -> do
            putStrLn "Opção inválida, tente novamente."
            telaLogin