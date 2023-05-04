fat :: Int -> Int
fat n 
    | n == 0 = 1
    | otherwise = n * fat (n - 1)

-- a. Receba um número n e retorne o enésimo número de Catalan. (1,5)
n_catalan:: Int -> Int 
n_catalan n 
    |n == 0 = 1
    |n == 1 = 1
    |n == 2 = 2
    |otherwise = div (fat (n * 2)) (fat (n + 1) * fat n)


-- b. Receba um número qualquer e informe se ele pertence à sequência ou não. (1,5)
aux:: Int -> Int -> Bool
aux x y
    |n_catalan x == y = True
    |n_catalan x > y = False
    |otherwise = aux x (y-1)


pertence_catalan:: Int -> Bool
pertence_catalan x = aux x 1

-- c. Contabilize quantos números da sequência existem abaixo de um determinado n. (1,5)




-- d. Calcule a soma de todos os números da sequência que aparecem em um intervalo
-- qualquer formado por dois números inteiros. (1,5)

