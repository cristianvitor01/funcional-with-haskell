--Função que soma 
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista(cabeca:cauda) = cabeca + somaLista cauda

--Função que inverte uma sequência de caracteres
inverte :: [Char] -> [Char]
inverte [] = []
inverte (a:as) = inverte as ++ [a]

--Função que verifica se duas lista são iguis
listasIguais :: [Int] -> [Int] -> Bool
listasIguais x y 
    |[x] == [y] = True
    |otherwise = False

--Função que ordena uma lista de inteiros
ordena :: [Int] -> [Int]
ordena [] = []
ordena (x: xs) = insere x (ordena xs)

insere :: Int -> [Int] -> [Int]
insere x[] = x:[]
insere x (cabeca:cauda)
    |x <= cabeca = x:cabeca:cauda
    |otherwise = cabeca:(insere x cauda)

-- Listas usando Expressões ZF

--Função que recebe uma lista retorna outra lista de números pares multiplicados por 2
geradores :: [Int] -> [Int]
geradores x = [2*a | a <- x, mod a 2 == 0]

--Função que recebe uma tupla e soma os valores de cada tupla
somaTupla :: [(Int, Int)] -> [Int]
somaTupla pares = [a + b | (a, b) <- pares]

geradorDuplo :: [Int] -> [Int] -> [(Int, Int)]
geradorDuplo x y = [(a, b) | a <- x, b <- y]

--Função que retorna uma lista de divisores de um número
zfDivisores :: Int -> [Int]
zfDivisores x = [a | a <- [1..(x-1)], mod x a == 0]

--Função que retorna uma lista de números perfeitos
zfPerfeitos :: Int -> Bool
zfPerfeitos n = sum(zfDivisores n) == n

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], zfPerfeitos x]

--Função que concatena listas
zfConcatena :: [[Int]] -> [Int]
zfConcatena x = [x | sub <- x, x <-sub]

--Função que retorna os números ímpares entre 1 e 100
zfImpar = [x | x <- [2 .. 99], impar x]

--Função que retorna os números pares entre 10 e 100
zfPar = [x | x <- [11 .. 99], par x]

--Função que retorna os números ímpares entre um 1 e X
imparN :: Int -> [Int]
imparN x = [x | x <- [1 .. x], impar x]

--Função que retorna os números entre 1 e X que são múltiplos de 3 e 5
zfMultiplos :: Int -> [Int]
zfMultiplos x = [x | x <- [1..x], mod x 3 == 0 && mod x 5 == 0]

--Função que retorna uma lista de tuplas de 1 a x contendo x e o seu respectivo quadrado
tuplaQuadrado :: Int -> [(Int, Int)]
tuplaQuadrado x = [(x, x*x) |x <- [1..x]]
--Função que retorna tuplas com índices de uma matriz 3 x 4
tuplaMatriz = [(x, y) | x <- [0..3], y <- [0..4]]

--Função que retorna uma matriz n x m
matriz :: Int -> Int -> [(Int, Int)]
matriz n m = [(n, m) | n <- [0..n], m <- [0..m]]

--Função que retorna uma lista da sequência de fibonacci até n
listaFibonacci :: Int -> [Int]
listaFibonacci x = [x | x <- [0..x], pertenceFibonacci x == True]

--Função que retorna uma lista dos n primeiros números da sequência de fibonacci
primeiroFibonacci :: Int -> [Int]
primeiroFibonacci x = [fibonacci x | x <- [0..x]]

--Função que replica um caractere inserido de acordo com o valor informado
replicaChar :: Char -> Int -> [Char]
replicaChar caractere numero 
    |numero == 0 = []
    |otherwise = caractere: replicaChar caractere(numero-1)

--Função que resolve o problema da torre de Hanoi
hanoi :: Int -> Int -> Int -> Int -> [String]
hanoi discos origem auxiliar destino
    |discos == 0 = []
    |discos == 1 = [show(origem) ++ "->" ++ show(destino)]
    |otherwise = ch1 ++ [show(origem) ++ "->" ++ show(destino)] ++ ch2
        where
            ch1 = hanoi(discos-1) origem destino auxiliar
            ch2 = hanoi(discos-1) auxiliar origem destino