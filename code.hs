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


