-- ex1
-- 1° jeito
or1::Bool->Bool->Bool
or1 b False = b
or1 False b = b

-- 2° jeito
or2::Bool->Bool->Bool
or2 _ True = True
or2 True _ = True
or2 False False = False 

-- 3° jeito
or3::Bool->Bool->Bool
or3 True True = True
or3 True False = True
or3 False True = True
or3 False False = False

--b
-- 1° jeito
or1c::Bool->Bool->Bool
or1c x y
    | x == y = x
    |otherwise = False

-- 2° jeito
or2c::Bool->Bool->Bool
or2c x y = if x == True then True
    else if y == True then True
    else False

-- ex2
dist_pontos::(Float,Float)->(Float,Float)->Float
dist_pontos (a,b) (c,d) = sqrt((c - a)**2 + (d - b)**2)

-- ex3
-- com guardas
fatorialg::Int->Int
fatorialg n
    | n == 0 = 1
    | otherwise = n*fatorialg(n-1)

-- com casamento de padrao
fatorialc::Int->Int
fatorialc 0 = 1
fatorialc n = n*fatorialc(n-1)

-- ex4
fibo::Int->Int
fibo n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibo(n-2) + fibo(n-1)

-- ex5
n_tri::Int->Int
n_tri n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = n + n_tri(n-1)

-- ex6
potencia2::Int->Int
potencia2 n
    | n == 0 = 1
    | n == 1 = 2
    | otherwise = 2 * potencia2(n-1)

-- ex7
-- a)
prodIntervalo::Int->Int->Int
prodIntervalo m n
    | m > n = 1
    | m == n = m
    | otherwise = m*(prodIntervalo (m+1)(n-1))*n

-- b)
fat::Int->Int
fat n = prodIntervalo 1 n


-- ex8
resto_div::Int->Int->Int
resto_div m n
    | n == 1 = m
    | m < n = m
    | otherwise = resto_div (m-n) n

div_inteira::Int->Int->Int
div_inteira m n
    | n > m = 0
    | n == m = 1
    | otherwise = 1+div_inteira (m-n) n

-- ex9
-- com guarda
mdcg::(Int,Int)->Int
mdcg (m,n)
    | n == 0 = m
    | otherwise = mdcg(n, (mod m n))

-- com casamento
mdc::(Int,Int)->Int
mdc (m,0) = m
mdc (0,n) = n
mdc (m,n) = mdc(n, (mod m n))

-- ex10
--com guarda
binog::(Int,Int)->Int
binog (n,k)
    | n < k = error"n < k"
    | k == 0 = 1
    | k == n = 1
    | otherwise = binog(n-1, k) + binog(n-1,k-1)

--com casamento
binoc::(Int,Int)->Int
binoc (n, 0) = 1
binoc (n, k) = if n < k then error" n < k "
    else if n == k then 1
    else binoc(n-1, k) + binoc(n-1, k-1)

-- ex11
passo::(Int,Int)->(Int,Int)
passo (x,y)
    | x == 0 && y == 0 = (1,1)
    | otherwise = (y, x+y)

fiboAux::Int->(Int,Int)
fiboAux n
    | n == 1 = passo(0,0)
    | otherwise = passo(fiboAux (n-1))

fibo2::Int->Int
fibo2 n = fst (fiboAux n)