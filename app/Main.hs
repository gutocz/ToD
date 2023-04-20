import System.IO
import Modules.Users.Users

main :: IO ()
main = do
    let result = createUser "joao123" "João Silva" "senhasupersegura" "Usuário ativo desde 2021"
    putStrLn result