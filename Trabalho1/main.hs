-- esse arquivo aqui foi utilizado para testar as funcoes que criei para o trabalho porque eu ainda nao sei
-- como puxar arquivos em outros arquivos em haskell

module Main where

matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta ([]:_) = []
matrizTransposta m = (map head m) : matrizTransposta (map tail m) 

main = do print(null (matrizTransposta []))