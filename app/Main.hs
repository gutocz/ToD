import System.IO
import Modules.Users.Users

main :: IO ()
main = do
    putStrLn "SALVE\nMENU FODA"
    putStrLn "Selecione uma opção:\n1.Cadastrar novo usuário\n2.Deletar usuário\n3.Exibir nome"
    a <- getLine
    let option = read a :: Int
    if option == 1
        then do
            putStrLn "Digite seu username: "
            name <- getLine
            putStrLn "Digite seu nome: "
            username <- getLine
            putStrLn "Digite sua senha: "
            password <- getLine
            putStrLn "Digite sua descrição: "
            desc <- getLine
            createUser name username password desc
    else
        if option == 2
            then do
                putStrLn "Digite o username da conta que quer deletar: "
                username <- getLine
                deleteUser username
        else
            if option == 3
                then do
                    putStrLn "Digite o username da conta que quer saber o nome: "
                    username <- getLine
                    name <- getName username
                    putStrLn name
            else
                putStrLn "Erro"
    