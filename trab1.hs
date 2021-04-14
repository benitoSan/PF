-- ex1
analisa_raizes::Float -> Float -> Float -> String
analisa_raizes a b c 
    | a == 0 = "4-equacao degenerada"
    | b^2 > 4*a*c = "1-possui duas raizes reais"
    | b^2 == 4*a*c = "2-possui uma raiz real"
    | otherwise = "3-nenhum raiz real"

-- ex2
calcula_delta::Float -> Float ->  Float -> Float
calcula_delta a b c = b^2 - (4*a*c)

equacao::Float -> Float -> Float -> (Float,Float)
equacao a b c = if a > 0 then ((-b + sqrt(calcula_delta a b c))/(2*a), (-b - sqrt(calcula_delta a b c))/(2*a))
    else (c/b, a)

-- ex3
type Data = (Int, Int, Int)

verifica_idade::Data -> Data -> Int
verifica_idade (da,ma,aa) (dn,mn,an) = if (ma - mn) < 0 then aa-an-1
    else if (ma - mn) >= 0 then aa-an
    else if (ma - mn) == 0 && (da - dn) >= 0 then aa-an-1
    else aa-an

passagem::Float -> Data -> Data -> Float
passagem preco (da,ma,aa) (dn,mn,an)
    | verifica_idade (da,ma,aa) (dn,mn,an) >= 70 = preco*0.5
    | verifica_idade (da,ma,aa) (dn,mn,an) < 2 = preco*0.15
    | verifica_idade (da,ma,aa) (dn,mn,an) <= 10 = preco*0.4
    | otherwise = preco

-- ex4
-- a)
gera1 = [x^3 | x <- [3..11], even x]

-- b)
gera2 = [(x,y) | x <- [1..20], y <- [x..(3*x)], x <= 5]

-- c)
l1 = [15,16]

gera3 = [x | x <- [1..(head l1)], x <- [1..(last l1)]]

-- d)
gera4 = [(x,x+1) | x <- [1..10], even x, x+1 <= 10]

-- e)
gera5 = [(x+y) | (x,y) <- gera4]


-- ex5
-- a)
contaPosM3 xs = length [x | x <- xs, mod x 3 == 0, x > 0]

-- b)
listaPosM3 xs = [x | x <- xs, mod x 3 == 0, x > 0]


-- ex6
fatores::Int -> [Int]
fatores n = [x | x <- [1..n], mod n x == 0]

eh_primo::Int -> Bool
eh_primo n = if fatores n == [1,n] then True
    else False

primos::Int -> Int -> [Int]
primos x y = [x | x <- [x..y], eh_primo x]


-- ex7
mdc::Int -> Int ->Int
mdc m 0 = m
mdc 0 n = n
mdc m n = mdc n (mod m n)

mmc_2num::Int -> Int -> Int
mmc_2num x y = if x == y then x
    else div (x*y) (mdc x y)

mmc::Int -> Int -> Int -> Int
mmc x y z = mmc_2num x (mmc_2num y z)

-- ex8
calcSerie::Float -> Int -> Float
calcSerie x 1  = 1/x
calcSerie x n = if odd n then fromIntegral(n)/x + (calcSerie x (n-1))
    else x/(fromIntegral(n)) + (calcSerie x (n-1))

-- ex9
escreve_fizzbuzz::Int -> String
escreve_fizzbuzz n
    | (mod n 2) == 0 && (mod n 3) == 0 = "FizzBuzz"
    | (mod n 2) == 0 = "Fizz"
    | (mod n 3) == 0 = "Buzz"
    | otherwise = "No"

fizzbuzz::Int -> [String]
fizzbuzz n = [escreve_fizzbuzz x | x <- [1..n]]

-- ex10
seleciona_multiplos::Int -> [Int] -> [Int]
seleciona_multiplos n xs = [x | x <- xs, mod x n == 0]

-- ex11
conta_elemento::Eq t => t -> [t] -> Int
conta_elemento _ [] = 0
conta_elemento e (x:xs)
    | e == x = (conta_elemento e xs) + 1
    | otherwise = conta_elemento e xs

unica_ocorrencia::Int -> [Int] -> Bool
unica_ocorrencia e xs
    | (conta_elemento e xs) == 1 = True
    | otherwise = False

-- ex12
intercala::[t] -> [t] -> [t]
intercala l [] = l
intercala [] l = l
intercala (x:xs) (y:ys) = x:y:(intercala xs ys)

-- ex13
zipar::[t] -> [t] -> [[t]]
zipar l [] = []
zipar [] l = []
zipar (x:xs) (y:ys) = [x,y]:(zipar xs ys)

-- ex14
type Nome = String
type Endereco = String
type Telefone = Int
type Email =  String
type Contato = (Nome, Endereco, Telefone, Email)
type Agenda = [Contato]

minha_agenda::Agenda
minha_agenda = 
    [("Joseph Joestar", "Avenida X", 9999999, "jojo@ba.com"),
     ("Robert Speedwagon", "Avenida Y", 8888888, "speedwagon@ba.com"),
     ("Gyro Zeppeli", "Avenida Z", 7777777, "zepp@ba.com")]

extrai_nome::Contato -> Nome
extrai_nome (nome, _, _, _) = nome

verifica_email::Email -> Contato -> Bool
verifica_email email_procurado (_,_,_,email_contato)
    | email_procurado == email_contato = True
    | otherwise = False


recuperaNome::Email -> Agenda -> Nome
recuperaNome _ [] = "Email desconhecido"
recuperaNome email (x:xs)
    | (verifica_email email x) == True = extrai_nome x
    | otherwise = recuperaNome email xs


-- ex15
type Pessoa = (String, Float, Int, Char)
pessoas::[Pessoa]
pessoas = 
    [("Rosa", 1.66, 27,'S'),
    ("João", 1.85, 26, 'C'),
    ("Maria", 1.55, 62, 'S'),
    ("Jose", 1.78, 42, 'C'),
    ("Paulo", 1.93, 25, 'S'),
    ("Clara", 1.70, 33, 'C'),
    ("Bob", 1.45, 21, 'C'),
    ("Rosana", 1.58,39, 'S'),
    ("Daniel", 1.74, 72, 'S'),
    ("Jocileide", 1.69, 18, 'S')]

conta_pessoas::[Pessoa] -> Float
conta_pessoas [] = 0
conta_pessoas (_:xs) = (conta_pessoas xs) + 1

extrai_altura::Pessoa -> Float
extrai_altura (_, altura, _, _) = altura

somatorio_alturas::[Pessoa] -> Float
somatorio_alturas [] = 0
somatorio_alturas (x:xs) = (extrai_altura x) + (somatorio_alturas xs)

altura_media::[Pessoa] -> Float
altura_media pessoas = (somatorio_alturas pessoas) / (conta_pessoas pessoas)

extrai_idade::Pessoa -> Int
extrai_idade (_, _, idade, _) = idade

compara_idade:: Pessoa -> Pessoa -> Pessoa
compara_idade pessoa1 pessoa2 = if extrai_idade pessoa1 > extrai_idade pessoa2 then pessoa2
    else pessoa1

busca_mais_novo::[Pessoa] -> Pessoa
busca_mais_novo [pessoa] = pessoa
busca_mais_novo (x:y:xs) = busca_mais_novo ((compara_idade x y):xs)

pessoa_mais_nova::[Pessoa] -> Int
pessoa_mais_nova pessoas = extrai_idade(busca_mais_novo pessoas)


compara_maior_idade:: Pessoa -> Pessoa -> Pessoa
compara_maior_idade pessoa1 pessoa2 = if extrai_idade pessoa1 > extrai_idade pessoa2 then pessoa1
    else pessoa2

busca_mais_velha::[Pessoa] -> Pessoa
busca_mais_velha [pessoa] = pessoa
busca_mais_velha (x:y:xs) = busca_mais_velha ((compara_maior_idade x y):xs)

extrai_nome_estadoCivil::Pessoa -> (String, Char)
extrai_nome_estadoCivil (nome, _, _, estadoCivil) = (nome, estadoCivil)

pessoa_mais_velha::[Pessoa] -> (String, Char)
pessoa_mais_velha pessoas = extrai_nome_estadoCivil(busca_mais_velha pessoas)

pessoas_50_mais::[Pessoa] -> [Pessoa]
pessoas_50_mais pessoas = [x | x <- pessoas, (extrai_idade x) >= 50]

conta_pessoas_casadas::[Pessoa] -> Int
conta_pessoas_casadas [] = 0
conta_pessoas_casadas (_:xs) = (conta_pessoas_casadas xs) + 1

extrai_estadoCivil::Pessoa -> Char
extrai_estadoCivil (_, _, _, estadoCivil) = estadoCivil

pessoas_casadas_aos_i::Int -> [Pessoa] -> [Pessoa]
pessoas_casadas_aos_i i pessoas = [x | x <- pessoas, (extrai_idade x) > i, (extrai_estadoCivil x) == 'C']

pessoas_casadas_idade_i::[Pessoa] -> Int -> Int
pessoas_casadas_idade_i pessoas i = conta_pessoas_casadas(pessoas_casadas_aos_i i pessoas)

-- ex16
insere_ord::Ord t => t -> [t] -> [t]
insere_ord e [] = [e]
insere_ord e (x:xs) = if e <= x then (e:x:xs)
    else x:(insere_ord e xs)

-- ex17
reverte::[t] -> [t]
reverte [x] = [x]
reverte (x:xs) = (reverte xs)++[x]

-- ex18
elimina_elem::Eq t => t -> [t] -> [t]
elimina_elem _ [] = []
elimina_elem e (x:xs)
    | e == x = elimina_elem e xs
    | otherwise = x:(elimina_elem e xs)

elimina_rep::Eq t => [t] -> [t]
elimina_rep [] = []
elimina_rep (x:xs) = x:(elimina_rep (elimina_elem x xs))

-- ex19
disponiveis = [1,2,5,10,20,50,100]

notasTroco::Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco n = [x:xs | x <- disponiveis, x <= n, xs <- notasTroco (n-x)]

teste::Int -> [[Int]]
teste 0 = [[]]
teste n = [x:xs | x <- [1..n], n >= x,xs <- teste (n-1)]

-- ex20
nRainhas :: Int -> [[Int]]
nRainhas n = gera_tab n [1..n]

remove_igual::Int -> [Int] -> [Int]
remove_igual _ [] = []
remove_igual x (y:ys) = if x == y then remove_igual x ys
    else (y:(remove_igual x ys))

testa_diag::Int -> [Int] -> Bool
testa_diag _ [] = True
testa_diag _ [_] = True
testa_diag n (x:y:xs)
    | abs(x-y) == n = False
    | otherwise = testa_diag (n+1) (x:xs)

gera_tab :: Int -> [Int] -> [[Int]]
gera_tab 0 _ = [[]]
gera_tab n coluna = [x:xs | x <- coluna, xs <- gera_tab (n-1) (remove_igual x coluna), testa_diag 1 (x:xs)]
