
tamanho :: [a] -> Int
tamanho [] = 0
tamanho l = 1 + tamanho (tail l)

eleva2 :: [Int] -> [Int]
eleva2 [] = []
eleva2 l = (head l)^2 : eleva2 (tail l)

contido :: Char -> String -> Bool
contido _ "" = False
contido c p = if (head p == c) then True else contido c (tail p)

-- 1.Defina uma função recursiva que receba uma lista de números inteiros e produza uma nova lista com cada número elevado ao quadrado.

elevaQuad :: [Int] -> [Int]
elevaQuad [] = []
elevaQuad l = (head l)^2 : elevaQuad (tail l)

-- 2.Escreva uma função recursiva que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.

addPref :: [String] -> [String]
addPref [] = []
addPref (x:xs) = ("Sr. "++x) : addPref xs

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

-- 8.Defina uma função não-recursiva que receba uma string, possivelmente contendo espaços, e que retorne outra string substituindo os demais caracteres por '-', mas mantendo os espaços. Exemplos:

-- codifica "Rio Grande do Sul"
--"--- ------ -- ---"
-- codifica ""
--""

--codifica :: String -> String
--codifica f = 

-- 9.Defina uma função recursiva que resolva o mesmo problema do exercício anterior.

codifica' :: String -> String
codifica' "" = ""
codifica' (x:xs) = if (x==' ') then x: codifica' xs else '-': codifica' xs

-- 10.Crie uma função recursiva charFound :: Char -> String -> Bool, que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento). Exemplos de uso da função:

-- charFound 'a' ""  
--False  
-- charFound 'a' "uau"  
--True  

charFound :: Char -> String -> Bool
charFound _ "" = False
charFound c (x:xs) = if (x==c) then True else charFound c xs

-- 11.Defina uma função recursiva que receba uma lista de coordenadas de pontos 2D e desloque esses pontos em 2 unidades, conforme o exemplo abaixo:

-- translate [(0.1,0.2), (1.1,6), (2,3.1)]
--[(2.1,2.2),(3.1,8.0),(4.0,5.1)] 

translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate (x:xs) = (fst x+2,snd x+2) : translate xs

-- 12.Defina uma função recursiva que receba 2 listas e retorne uma lista contendo o produto, par a par, dos elementos das listas de entrada. Exemplos:

-- prodVet [1,2,3] [4,5,6]
--[4,10,18]
-- prodVet [1,2,3] [4,5,6,7]
--[4,10,18]

prodVet :: [Int] -> [Int] -> [Int]
prodVet [] _ = []
prodVet _ [] = []
prodVet (x:xs) (y:ys) = x*y : prodVet xs ys

-- 13.Resolva o exercício anterior usando uma função de alta ordem, eliminando a necessidade de escrever código com recursão.

prodVet' :: [Int] -> [Int] -> [Int]
prodVet' l1 l2 = zipWith (*) l1 l2

-- 14.Defina uma função recursiva que receba um número n e retorne uma tabela de números de 1 a n e seus quadrados, conforme os exemplos abaixo:

-- geraTabela 5
--[(1,1),(2,4),(3,9),(4,16),(5,25)]
-- geraTabela 0
--[]

geraTabela :: Int -> [(Int,Int)]
geraTabela n = geraTabela' n 1

geraTabela' :: Int -> Int -> [(Int,Int)]
geraTabela' n i = if (i <= n) then (i,i^2): geraTabela' n (i+1) else []