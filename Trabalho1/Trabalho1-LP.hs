-- esse aqui eh o arquivo submetido na plataforma CodeBoard que compreende o trabalho
-- Nome: Maria Eduarda Carvalho Santos

module Root.Exercicios.Exercicios where

import Data.List (sortBy)

-- 1) (Valor da questão: 1,0 ponto) 
-- Defina uma função que retorne o maior entre quatro inteiros.
maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d = max (max a b) (max c d)

-- 2) (Valor da questão: 1,0 ponto) 
-- Defina uma função que receba uma nota e retorne a menção do aluno.
-- Não se preocupe com a validação do input. A nota sempre será um Número entre 0.0 (inclusive) e 10.0 (inclusive).
-- Considere a seguinte tabela para tradução da menção:
-- De 9 a 10 -> "SS"
-- De 7 a 8.9 -> "MS"
-- De 5 a 6.9 -> "MM"
-- De 3 a 4.9 -> "MI"
-- De 0.1 a 2.9 -> "II"
-- De 0 -> "SR"
converterNotaParaMencao :: Float -> String
converterNotaParaMencao n
                    | n == 0.0          = "SR"
                    | n>0.0 && n<3.0    = "II"
                    | n>=3.0 && n<5.0   = "MI"
                    | n>=5.0 && n<7.0   = "MM"
                    | n>=7.0 && n<9.0   = "MS"
                    | otherwise         = "SS"

-- 3) (Valor da questão: 1,0 ponto) 
-- defina uma função que retorna um booleano indicando se uma lista de inteiros é decrescente ou não:
isDecrescente :: [Int] -> Bool
isDecrescente [] = True
isDecrescente [x] = True
isDecrescente (x1:x2:xs) = if x1 > x2 then isDecrescente (x2:xs) else False

-- 4) (Valor da questão: 2,0 pontos) 
-- defina uma função que recebe uma lista de strings como entrada e computa uma lista de pares 
-- de (String, Int) representando o histograma de seus elementos:
histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma (s:ss) = (s, length(filter (==s) ss)+1): histograma (filter (/=s) ss)


-- 5)(Valor da questão: 1,5 ponto) 
-- Defina a função myZipWith que tem como parâmetros uma função binária (que tem dois parâmetros) e duas listas, 
-- retornando uma lista de valores resultantes da aplicação dessa função nos elementos correspondentes dessas 
-- listas:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith (+) [] _ = []
myZipWith (+) _ [] = []
myZipWith (+) (a:as) (b:bs) =  ((+) a b) : (myZipWith (+) as bs)


-- 6) (Valor da questão: 2,0 ponto) 
-- Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,
-- determinar a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar
-- ordenado crescentemente pela média aritmética das notas. A aprovação ocorre se, e somente se, tal
-- média é maior ou igual a cinco.
-- OBSERVAÇÃO: especificamente para este exercício, você pode importar as funções de ordenaçao de listas (como 'sort' ou 'sortBy') se achar necessário.


aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia [] = []
-- eu decidi filtrar primeiro antes de ordenar porque existirao menos elementos para ordenar com a aplicacao da funcao filter
aprovadosOrdemDeMedia ((name, n1, n2):pls) = sortBy (\(_, y1) (_,y2) -> if y1>=y2 then GT else LT) (filter (\(_, y) -> y >= 5.0) ((name, nf) : aprovadosOrdemDeMedia pls))
                                            where nf = (n1+n2)/2

-- 7) (Valor da questão: 1,5 ponto, sendo 0.5 ponto para cada letra) 
-- Considere a representação de matrizes como lista de listas em que cada elemento da lista é uma lista 
-- que representa uma linha da matriz. Com base nisso, determine as seguintes funções:
--  a) some duas matrizes
--  b) compute a transposta de duas matrizes 
--  c) compute a multiplicação de duas matrizes
-- OBSERVAÇÃO: considere que os inputs são válidos (ou seja, as matrizes são válidas e as suas dimensões são compatíveis para soma e multiplicação)

-- Todos esses exercicios foram feitos com a ajuda do monitor (eu realmente nao sabia como faze-los)
somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial ma mb = myZipWith (myZipWith (+)) ma mb

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta ([]:_) = []
matrizTransposta m = (map head m) : matrizTransposta (map tail m) 

multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial [] _ = []
multiplicacaoMatricial m1 m2 = [[sum $ myZipWith (*) m1Linha m2Coluna | m2Coluna <- (matrizTransposta m2)] | m1Linha <- m1]
