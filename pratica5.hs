-- ex1
conta_ch::[Char] -> Int
conta_ch [] = 0
conta_ch (x:xs) = conta_ch xs + 1

conta::[t] -> Int
conta [] = 0
conta (_:xs) = conta xs + 1

maior::[Int] -> Int
maior [x] = x
maior (x:y:t)
    | x > y = maior (x:t)
    | otherwise = maior (y:t)

primeiros::Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x:primeiros (n-1) xs

pertence::Eq t => t -> [t] -> Bool
pertence e [] = False
pertence e (x:xs) = if (x == e) then True
    else pertence e xs

uniaoR::Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l then uniaoR xs l
    else (x:uniaoR xs l)

-- ex2
npares::[Int] -> Int
npares [] = 0
npares (x:xs) = if even x then (npares xs) + 1
    else npares xs

-- ex3
list_prod::[Int] -> Int
list_prod [x] = x
list_prod (x:xs) = x * (list_prod xs)

-- ex4
comprime::[[t]] -> [t]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

-- ex5
tam::[Int] -> Int
tam [] = 0
tam (_:xs) = tam xs + 1

-- ex6
-- Usa pertence do ex1
uniaoRec2::Eq t => [t] -> [t] -> [t]
uniaoRec2 l [] = l
uniaoRec2 l (x:xs) = if pertence x l then uniaoRec2 l xs
    else uniaoRec2 (l++[x]) xs