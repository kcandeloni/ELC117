-- 1.Escreva uma função addSuffix :: String -> [String] -> [String] usando list comprehension, para adicionar um dado sufixo às strings contidas numa lista. Exemplo:

-- addSuffix "@inf.ufsm.br" ["fulano","beltrano"]
--["fulano@inf.ufsm.br","beltrano@inf.ufsm.br]

addSuffix :: String -> [String] -> [String]
addSuffix suf l = [ x++suf | x<-l]

-- 2.Escreva uma função countShorts :: [String] -> Int, que receba uma lista de palavras e retorne a quantidade de palavras dessa lista que possuem menos de 5 caracteres. Use recursão.

countShots :: [String] -> Int
countShots [] = 0
countShots (x:xs) = if ((length x) < 5) then 1 + countShots xs else countShots xs

-- 3.Reescreva a função do exercício acima, desta vez usando list comprehension.

countShots' :: [String] -> Int
countShots' l = length [ x | x <- l, (length x)<5]

-- 4.Escreva uma função ciclo :: Int -> [Int] -> [Int] que receba um número N e uma lista de inteiros, retornando uma nova lista com N repetições da lista original, conforme o exemplo abaixo:

-- ciclo 4 [1,3]
--[1,3,1,3,1,3,1,3]
--Obs.: Você deve usar recursão neste exercício.

ciclo :: Int -> [Int] -> [Int]
ciclo 0 _ = []
ciclo n l = l++ ciclo (n-1) l

-- 5.Escreva uma função numera :: [String] -> [(Int,String)], que receba uma lista de palavras e retorne outra lista contendo tuplas com as palavras numeradas a partir de 1. Use recursão. Exemplo de uso da função:

-- numera ["abacaxi","mamao","banana"]
--[(1,"abacaxi"),(2,"mamao"),(3,"banana")]

numera :: [String] -> [(Int,String)]
numera l = numera' l 1

numera' :: [String] -> Int -> [(Int,String)]
numera' [] _ = []
numera' (x:xs) n = (n,x) : numera' xs (n+1)

-- 6.Explique, em forma de comentário, o resultado de cada expressão abaixo.

--a) [ (x,y) | x <- [1..5], even x, y <- [(x + 1)..6], odd y ]
-- Sera gerada uma lista de tuplas, 
--b) [ a ++ b | a <- ["lazy","big"], b <- ["frog", "dog"]]
-- 
--c) concat [ [a,'-'] | a <- "paralelepipedo", not (elem a "aeiou")]
-- 

-- 7.(G. Malcolm, Univ. Liverpool) Write a function crossProduct :: [a] -> [b] -> [(a,b)] that takes two lists xs and ys, and returns the list of all possible pairings:

--[ (x,y) | x <- xs, y <- ys ]
--without using the above list comprehension. (As an exercise in problem decomposition, try first defining a "helper" function pairWithAll :: a -> [b] -> [(a,b)] that pairs its first argument with each element in its second.)

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct [] _ = []
crossProduct (x:xs) l2 = pairWithAll x l2 ++ crossProduct xs l2

pairWithAll :: a -> [b] -> [(a,b)]
pairWithAll _ [] = []
pairWithAll s (y:ys) = (s,y) : pairWithAll s ys

-- 8.
