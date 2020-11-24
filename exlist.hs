{--#Ímpares entre 1 e 100--}
lstImpar = [e|e <- [1..100], e `mod` 2 /= 0]

{--#Pares entre 10 e 100--}
lstPar = [e|e <- [10..100], e `mod` 2 == 0]

{--#Ímpares entre 1 e N--}
geraImpares :: Int -> [Int]
geraImpares n = [e|e <- [1..n], e `mod` 2 /= 0]

{--#Números entre 1 e N que são múltiplos de 3 e 5 ao mesmo tempo.--}
listMultiplos :: Int -> [Int]
listMultiplos n = [e|e <- [1..n], e `mod` 3 == 0 , e `mod` 5 == 0]

{--#Tuplas entre 1 e N, contendo o número e seu respectivo quadrado.--}
listTuplas :: Int -> [(Int,Int)]
listTuplas n = [(e,eq)|e <- [1..n], eq<-[e*e]]

{--#Tuplas com os indices de uma matriz 3x4.--}
listTuplasMatriz = [(n,m)|n <- [1..3], m<-[1..4]]

{--#Tuplas com os indices de uma matriz NxM.--}
listTuplasMatrizNxM :: Int -> Int -> [(Int,Int)]
listTuplasMatrizNxM n m = [(i,j)|i <- [1..n], j<-[1..m]]

{--#Escreva uma função com a seguinte assinatura listaFibonacci :: Int->[Int] que retorna uma lista com
os n primeiros números da sequência de Fibonacci.--}

listaFibonacci :: Int -> [Int]
listaFibonacci 0 = []
listaFibonacci 1 = [fib 1]
listaFibonacci n = listaFibonacci (n-1) ++ [fib n]


fib :: Int -> Int
fib n
    |n == 0 = 0
    |n == 1 = 1
    |otherwise = fibAux (n-1) 0 1


fibAux :: Int -> Int -> Int -> Int
fibAux n ant atu
    |n == 0 = atu
    |otherwise = fibAux (n - 1) (atu) (ant+atu)


{--#O sistema de numeração hexadecimal é muito utilizado para representar números binários de uma
forma mais compacta, pois é muito fácil converter binários para hexadecimal e vice-versa. Dessa
forma, esse sistema é bastante utilizado em aplicações de computadores e microprocessadores.
Sabemos que o sistema de numeração binário (base 2) possui apenas dois valores (0 e 1) para
cada casa numérica. Já o sistema de numeração hexadecimal (base 16) possui dezesseis
possíveis valores (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F). Escreva uma função em Haskell que
receba uma String em binário e retorne uma String com sua representação hexadecimal.--}

converteBinHex :: [Char] -> [Char]
converteBinHex num = converteDecHex(converteBinDec num)


converteBinDec :: [Char] -> Int
converteBinDec [] = 0
converteBinDec (h:t) = convertidoDec + converteBinDec t
    where
        convertido = ( read [h] :: Int)
        convertidoDec = (convertido)*2^((length t))

converteDecHex :: Int -> [Char]
converteDecHex n = if n<=16 
    then mapHex n 
    else (converteDecHex (n `div` 16))
        ++ mapHex(n `mod` 16)
    

mapHex :: Int -> [Char]
mapHex n
    | n == 10 = ['A']
    | n == 11 = ['B']
    | n == 12 = ['C']
    | n == 13 = ['D']
    | n == 14 = ['E']
    | n == 15 = ['F']
    | otherwise = show n