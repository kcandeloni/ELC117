--1.  Crie uma função `somaQuad :: Int -> Int -> Int` que calcule a soma dos quadrados de dois números x e y.

somaQuad :: Int -> Int -> Int
somaQuad x y =  x^2 + y^2

-- 2. Crie uma função `hasEqHeads :: [Int] -> [Int] -> Bool` que verifique se 2 listas possuem o mesmo primeiro elemento. Use o operador lógico '==' para verificar igualdade.

hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads l1 l2 = if head l1 == head l2 then True else False

-- 3. Escreva uma função que receba uma lista de nomes e adicione a string "Sr. " no início de cada nome.

addPref :: [String] -> [String]
addPref l = map ("Sr. "++) l

-- 4. Crie uma função que receba uma string e retorne o número de espaços nela contidos. Dica: aplique 2 funções consecutivamente.

contEsp :: String -> Int
contEsp w = length (filter (==' ') w)

-- 5. Escreva uma função que, dada uma lista de números, calcule 3*n^2 + 2/n + 1 para cada número n da lista. Dica: defina uma função anônima.

quadrado :: Int -> Int
quadrado n = n*n

calcList :: [Int] -> [Int]
calcList l = map (\n -> 3 * quadrado n + div 2 n + 1) l

--6. Escreva uma função que, dada uma lista de números, selecione somente os que forem negativos.

selectNegative :: [Int] -> [Int]
selectNegative l = filter (<0) l

--7. Escreva uma função que receba uma lista de números e retorne somente os que estiverem entre 1 e 100, inclusive. Dica 1: use uma função anônima. Dica 2: use o operador '&&' para expressar um 'E' lógico.

selectInterv :: [Int] -> [Int]
selectInterv l = filter (\n -> n>=1 && n<=100) l

--8. Escreva uma função que, dada uma lista de idades de pessoas no ano atual, retorne uma lista somente com as idades de quem
--nasceu depois de 1970. Para testar a condição, sua função deverá subtrair a idade do ano atual.

verifIdade :: [Int] -> [Int]
verifIdade l = filter (\n -> (2016-n) > 1970) l

--9. Escreva uma função que receba uma lista de números e retorne somente aqueles que forem pares.

par :: Int -> Bool
par n = if mod n 2 == 0 then True else False

checkPair ::  [Int] -> [Int]
checkPair l = filter par l

--10. Crie uma função `charFound :: Char -> String -> Bool` que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento). Exemplos de uso da função:

 --  ```
 --  > charFound 'a' ""  
 --  False  
 --  > charFound 'a' "uau"  
 --  True  
 --  ```
 
charFound :: Char -> String -> Bool
charFound c p = if(length (filter (==c) p) > 0) then True else False

--11. A função `takeWhile :: (a -> Bool) -> [a] -> [a]` é uma função de alta ordem. Ela recebe uma função condicional e uma lista, retornando o "menor prefixo" (isto é, porção inicial) da lista que satisfaça a condição dada. Teste os exemplos abaixo no GHCi e depois crie um novo exemplo:

 --  ```
 --  > takeWhile (< 5) [1,2,3,4,5]
 --  > takeWhile (/=' ') "Fulana de Tal"
 --  ```
 --  Obs.: Este exercício deve ser entregue em forma de comentário.
 
 --takeWhile (\x -> x*x < 500) [19,34,54,67]
 
--12. Crie uma função que receba uma lista de nomes e retorne outra lista com somente aqueles nomes que terminarem com a letra 'a'.

nameLastA :: [String] -> [String]
nameLastA l = filter (\name -> (last name)=='a') l