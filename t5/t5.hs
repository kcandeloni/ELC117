import Data.Char
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
-- Gera uma lista de tuplas onde para cada x (lista de 1 a 5) par, forma uma tupla com y impar que pode assumir valores de x+1 até 6,
-- já que x inicia em 1, y vai de  2 a 6.
--b) [ a ++ b | a <- ["lazy","big"], b <- ["frog", "dog"]]
-- Concatena cada String da lista a com cada lista da lista b.
--c) concat [ [a,'-'] | a <- "paralelepipedo", not (elem a "aeiou")]
-- Cria uma lista de Strings com as consoantes da String "paralelepipeto" e o caracter '-', e depois os concatena,
-- formando uma lista com as consoantes da palavra separados por traço.  

-- 7.(G. Malcolm, Univ. Liverpool) Write a function crossProduct :: [a] -> [b] -> [(a,b)] that takes two lists xs and ys, and returns the list of all possible pairings:

--[ (x,y) | x <- xs, y <- ys ]
--without using the above list comprehension. (As an exercise in problem decomposition, try first defining a "helper" function pairWithAll :: a -> [b] -> [(a,b)] that pairs its first argument with each element in its second.)

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct [] _ = []
crossProduct (x:xs) l2 = pairWithAll x l2 ++ crossProduct xs l2

pairWithAll :: a -> [b] -> [(a,b)]
pairWithAll _ [] = []
pairWithAll s (y:ys) = (s,y) : pairWithAll s ys

-- 8.Nesta questão você deverá usar list comprehension. Suponha que um retângulo seja representado por uma tupla (Float,Float,Float,Float), contendo respectivamente as coordenadas x e y do ponto no seu canto superior esquerdo, seguidas das suas medidas de largura e altura. Sabendo que o eixo x --cresce de cima para baixo e o eixo y da esquerda para direita, crie uma função genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)] que receba um número N e um ponto (x,y) e gere uma sequência de N retângulos não sobrepostos. Os retângulos devem ser alinhados pelos seus topos, a partir do --ponto dado, com largura e altura constantes. Por exemplo, usando largura e altura iguais a 5.5:

-- genRects 3 (0,0) 
--[(0.0,0.0,5.5,5.5),(5.5,0.0,5.5,5.5),(11.0,0.0,5.5,5.5)]
--Obs.: Use conversão explícita de tipos quando misturar Int e Float.

genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)]
genRects n p = [ (x,realToFrac (snd p),5.5,5.5) | 
     x <- [realToFrac (fst p),((realToFrac (fst p))+5.5)..((realToFrac (fst p))+(5.5*(realToFrac (n-1))))]]

-- 9.Escreva uma função recursiva que receba uma lista de tuplas e decomponha cada uma delas, gerando uma tupla de listas, conforme o exemplo abaixo:

-- func [(1,3),(2,4)]
--([1,2], [3,4])

decompTupla :: [(Int,Int)] -> ([Int],[Int])
decompTupla t = (decompTupla' t,decompTupla'' t)

decompTupla' :: [(Int,Int)] -> [Int]
decompTupla' [] = []
decompTupla' (x:xs) = (fst x) : decompTupla' xs

decompTupla'' :: [(Int,Int)] -> [Int]
decompTupla'' [] = []
decompTupla'' (y:ys) = (snd y) : decompTupla'' ys

-- 10.Refaça o exercício anterior usando list comprehension.

decompTuplalc :: [(Int,Int)] -> ([Int],[Int])
decompTuplalc t = ([ fst x | x <- t],[ snd x | x <- t])

-- 11. Refaça o exercício anterior usando uma função de alta ordem.

decompTuplaao :: [(Int,Int)] -> ([Int],[Int])
decompTuplaao t = (map (\x -> fst x) t, map (\y -> snd y) t)

-- 12.O código em validaCPF.hs ilustra a validação dos dígitos verificadores de um CPF. Este código usa let para definir subexpressões, isto é, expressões intermediárias que irão compor o resultado da função. Observe que este código tem trechos um tanto repetitivos para calcular o primeiro e o segundo dígitos. Você deverá reescrever este código, criando uma função auxiliar que será chamada 2 vezes dentro de isCpfOk. Nessa função auxiliar, você deverá usar where para definir subexpressões.

{- validaCPF.hs
   Programa em Haskell para validar os digitos de um CPF
   Mais info em: http://pt.wikipedia.org/wiki/Cadastro_de_Pessoas_F%C3%ADsicas

import Data.Char

isCpfOk :: [Int] -> Bool
isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      expr1 = (sum $ zipWith (*) digitos1 [10,9..2]) `mod` 11
      dv1 = if expr1 < 2 then 0 else 11-expr1

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
      expr2 = (sum $ zipWith (*) digitos2 [11,10..2]) `mod` 11
      dv2 = if expr2 < 2 then 0 else 11-expr2
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

main = do
  let cpf = "12345678909"
      digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)
  -}

isCpfOk :: [Int] -> Bool
isCpfOk cpf = if checkCpf (take 9 cpf) == cpf !! 9 &&
   checkCpf (take 10 cpf) == cpf !! 10 then True else False

checkCpf :: [Int] -> Int
checkCpf cpf = if expr < 2 then 0 else 11-expr
   where n = (length cpf)+1
         expr = (sum $ zipWith (*) cpf [n,(n-1)..2]) `mod` 11

main = do
  let cpf = "12345678909"
      digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)