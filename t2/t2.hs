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

