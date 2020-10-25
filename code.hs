type Soma = (Int,Int)

soma:: Soma -> Int
soma (a,b) = a + b

{--#################################################--}

fib :: Int -> Int
fib n
    |n == 0 = 0
    |n == 1 = 1
    |otherwise = fibAux (n-1) 0 1


fibAux :: Int -> Int -> Int -> Int
fibAux n ant atu
    |n == 0 = atu
    |otherwise = fibAux (n - 1) (atu) (ant+atu)


{--#################################################--}
{--################## Forma pensada ###############################--}
ehPrimo :: Int -> Bool
ehPrimo n
    | n == 0 = False
    | n == 1 = False
    | otherwise = verificaPrimo n (div n 2) 1

verificaPrimo :: Int -> Int -> Int -> Bool
verificaPrimo n divi contDiv
    | contDiv > 2 = False
    | (divi == 0) && (contDiv == 2) = True
    | mod n divi == 0 = verificaPrimo n (divi - 1) (contDiv + 1)
    | mod n divi /= 0 = verificaPrimo n (divi - 1) contDiv
    | otherwise = False

{--#################################################--}
{--################# Forma pegada ################################--}
primo :: Int -> Bool
primo n
    |n == 1 = False
    |n == 2 = True
    |otherwise = auxPrimo n (n-1)

auxPrimo :: Int -> Int -> Bool
auxPrimo n d
    |d == 1 = True
    |mod n d == 0 = False
    |otherwise = auxPrimo n (d-1)


{--#################################################--}


potencia :: Int -> Int -> Int
potencia n expo
    | expo == 0 = 1
    | otherwise = n * potencia n (expo-1)


{--#################################################--}

nPrimo :: Int -> Int
nPrimo n = auxNPrimo n 1

auxNPrimo :: Int -> Int -> Int
auxNPrimo n numPrimo
    |n == 0 = numPrimo - 1
    |primo numPrimo = auxNPrimo (n - 1) (numPrimo + 1)
    |otherwise = auxNPrimo n (numPrimo + 1)


{--#################################################--}


fibonacciPrimo :: Int -> Int
fibonacciPrimo n = auxFibonacciPrimo n 1

auxFibonacciPrimo :: Int -> Int -> Int
auxFibonacciPrimo n nPrimoFib
    |n == 0 = fib (nPrimoFib - 1)
    |primo (fib nPrimoFib) = auxFibonacciPrimo (n-1) (nPrimoFib + 1)
    |otherwise = auxFibonacciPrimo n (nPrimoFib + 1)


{--#################################################--}

ehPerfeito :: Int -> Bool
ehPerfeito n
    | n == 0 = False
    | n == 1 = False
    | n == (auxEhPerfeito n (n `div` 2)) = True
    | otherwise = False

auxEhPerfeito :: Int -> Int -> Int
auxEhPerfeito n d
    | d == 1 = 1
    | n `mod` d == 0 = d + auxEhPerfeito n (d - 1)
    | otherwise = auxEhPerfeito n (d - 1)


{--#################################################--}

tamCiclo :: Int -> Int
tamCiclo n = auxSequencia n n 0


auxSequencia :: Int -> Int -> Int -> Int
auxSequencia n num ciclo
    | num == 1 = ciclo + 1
    | num `mod` 2 == 0 = auxSequencia (n - 1) (num `div` 2) (ciclo + 1)
    | otherwise = auxSequencia (n - 1) ((num * 3) + 1) (ciclo + 1)


{--#################################################--}


