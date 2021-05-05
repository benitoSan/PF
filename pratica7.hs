-- ex1
paridade::[Int] -> [Bool]
paridade l = map (even) l


-- ex2
prefixos::[String] -> [String]
prefixos l = map (take 3) l

-- ex3
saudacao::[String] -> [String]
saudacao l = map ("Oi "++) l


-- ex4
filtrar::(a -> Bool) -> [a] -> [a]
filtrar f xs = [x | x <- xs, f x]


-- ex5
pares::[Int] -> [Int]
pares l = filter (even) l


-- ex6
solucoes::[Int] -> [Int]
solucoes l = filter (calcula) l

calcula::Int -> Bool
calcula val = (\x -> 5*x + 6) val < (\x -> x*x) val


-- ex7
maior::[Int] -> Int
maior l = foldr1 (comp) l

comp::Int -> Int -> Int
comp x y = if x > y then x
    else y


-- ex8
menor_min10::[Int] -> Int
menor_min10 l = foldr1 (comp2) l

comp2::Int -> Int -> Int
comp2 x y = if x > y && y <= 10 then y
    else 
        if x <= 10 then x
        else 10
    

-- ex9
junta_silabasplural1::[String] -> String
junta_silabasplural1 l = foldr (++) "s" l


-- ex10
menores10::[Int] -> ([Int], Int)
menores10 l = execute l 0 []

execute::[Int] -> Int -> [Int] -> ([Int], Int)
execute [x] n ys = if x < 10 then ((concatena ys x),n+1)
    else (ys,n)
execute (x:xs) n ys = if x < 10 then execute (xs) (n+1) (concatena ys x)
    else execute xs n ys

concatena::[Int] -> Int -> [Int]
concatena [] e = [e]
concatena (x:xs) e = x:(concatena xs e)

-- ex11
busca_elem::Int -> [Int] -> (Bool, Int)
busca_elem n l = busca_elem1 n l 0

busca_elem1::Int -> [Int] -> Int -> (Bool, Int)
busca_elem1 _ [] count = (False, count)
busca_elem1 n (x:xs) count = if n == x then (True, count+1)
    else busca_elem1 n xs (count+1)