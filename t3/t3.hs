
tamanho :: [a] -> Int
tamanho [] = 0
tamanho l = 1 + tamanho (tail l)

eleva2 :: [Int] -> [Int]
eleva2 [] = []
eleva2 l = (head l)^2 : eleva2 (tail l)

contido :: Char -> String -> Bool
contido _ "" = False
contido c p = if (head p == c) then True else contido c (tail p)

translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate (x:xs) = (fst x+2,snd x+2) : translate xs

geraTabela' :: Int -> [(Int,Int)]
geraTabela' n = geraTabela n 1

geraTabela :: Int -> Int -> [(Int,Int)]
geraTabela n i = if (i <= n) then (i,i^2): geraTabela n (i+1) else []

-- 1.Defina uma função recursiva que receba uma lista de números inteiros e produza uma nova lista com cada número elevado ao quadrado.

elevaQuad :: [Int] -> [Int]
elevaQuad [] = []
elevaQuad l = (head l)^2 : elevaQuad (tail l)

-- 2.Escreva uma função recursiva que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.

addPref :: [String] -> [String]
addPref [] = []
addPref (x:xs) = ("Sr. " ++ x) : addPref xs

-- 3.Crie uma função recursiva que receba uma string e retorne o número de espaços nela contidos.

contEsp :: String -> Int
contEsp "" = 0
contEsp (x:xs) = if (x==' ') then 1 + contEsp xs else contEsp xs

-- 4.Escreva uma função recursiva que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista.

calcN :: [Int] -> [Int]
calcN [] = []
calcN (n:ns) = 3 * n^2 + div 2 n + 1 : calcN ns

-- 5.Escreva uma função recursiva que, dada uma lista de números, selecione somente os que forem negativos.

selecNeg :: [Int] -> [Int]
selecNeg [] = []
selecNeg (n:ns) = if (n < 0) then n: selecNeg ns else selecNeg ns

-- 6.Defina uma função não-recursiva que receba uma string e retire suas vogais, conforme os exemplos abaixo.

-- semVogais "andrea"
--"ndr"
-- semVogais "xyz"
--"xyz"
-- semVogais "ae"
--""

semVogais :: String -> String
semVogais p = filter (\l->l/='a'&&l/='e'&&l/='i'&&l/='o'&&l/='u') p

-- 7.Expresse uma solução recursiva para o exercício anterior.

semVogais' :: String -> String
semVogais' "" = ""
semVogais' (x:xs) = if (contido x "aeiou") then semVogais' xs else x : semVogais' xs

--