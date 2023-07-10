{-- Avaliação 03 --}
{-- Sistema em Haskell que auxilia no recenseamento da população --}


-- Definição de tipos
data Endereco = Endereco { rua :: String, casa :: Int, cidade :: String } deriving (Show, Eq)
data Pessoa = Pessoa { nome :: String, idade :: Int, endereco :: Endereco } deriving (Show, Eq)


-- Cadastro de Pessoas
cadastrarPessoa :: [Pessoa] -> Pessoa -> [Pessoa]
cadastrarPessoa [] p = [p]
cadastrarPessoa (x:xs) p
    | (nome x) == (nome p) = p:xs
    | otherwise = x:(cadastrarPessoa xs p)


-- Exibir o cadastro de pessoas
exibirCadastro :: [Pessoa] -> IO ()
exibirCadastro [] = putStrLn "Ainda não há pessoas cadastradas."
exibirCadastro (x:xs) = do
    putStrLn ("Nome: " ++ (nome x))
    putStrLn ("Idade: " ++ (show (idade x)))
    putStrLn ("Rua: " ++ (rua (endereco x)))
    putStrLn ("Casa: " ++ (casa (endereco x)))
    putStrLn ("Cidade: " ++ (cidade (endereco x)))
    putStrLn ""
    exibirCadastro xs


-- Localizar pessoas cadastradas
localizarPessoa :: [Pessoa] -> String -> Pessoa
localizarPessoa [] _ = putStrLn "Pessoa não encontrada."
localizarPessoa (x:xs) nomeBuscado
    | nome x == nomeBuscado = x
    | otherwise = localizarPessoa xs nomeBuscado


-- Atualizar cadastro de pessoas
atualizarCadastro :: [Pessoa] -> Pessoa -> [Pessoa]
atualizarCadastro [] p = [p]
atualizarCadastro (x:xs) p
    | nome x == nome p = p : xs
    | nome x > nome p = p : x : xs
    | otherwise = x : atualizarCadastro xs p


-- Relatório total de pessoas por cidade
relatorioPorCidade :: [Pessoa] -> String -> Int
relatorioPorCidade [] _ = 0
relatorioPorCidade (x:xs) cidade
    | cidade == cidadeAtual = 1 + relatorioPorCidade xs cidade
    | otherwise = relatorioPorCidade xs cidade
    where cidadeAtual = cidade (endereco x)


-- Calcular média de idade
mediaIdade :: [Pessoa] -> Double
mediaIdade [] = 0
mediaIdade pessoas = (somaIdades pessoas) /  (length pessoas)


-- Soma das idades
somaIdades :: [Pessoa] -> Int
somaIdades [] = 0
somaIdades (x:xs) = idade x + somaIdades xs
