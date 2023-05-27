module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}


{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qt est |verifica med est  qt == True = addQuant med est qt
                              |verifica med est  qt == False = empty med qt  est

empty :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos -- adiciona o medicamento na lista 
empty med qt [] = [(med,qt)]
empty med qt  (estoque:estoques) | (estoque:estoques) /= [] = [(med,qt)]++(estoque:estoques)

addQuant :: Medicamento  ->EstoqueMedicamentos-> Quantidade -> EstoqueMedicamentos -- adiciona a quantidade a medicamentos existentes 
addQuant med (estoque:estoques)  qt | med == fst estoque = (med,snd estoque + qt ): estoques
                                  | otherwise = [estoque] ++ addQuant med estoques qt

verifica :: Medicamento -> EstoqueMedicamentos -> Quantidade -> Bool -- verifica se o medicamento esta no estoque
verifica _ [] _ = False
verifica med  (estoque:estoques) qt | med == fst estoque =True
                                    | estoques == [] = False
                                    | otherwise = verifica med estoques qt


{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med est| verifica med est 0 == False = Nothing
                        | verifica med est 0 == True = listToMaybe med est est

listToMaybe :: Medicamento  -> EstoqueMedicamentos -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos -- verifica se a quant do medicamento é 0 
listToMaybe med est (estoque:estoques)| med == fst estoque && 0 == snd estoque = Nothing
                                       | med /= fst estoque = listToMaybe med est estoques
                                       | otherwise = Just (certfy med est)

certfy :: Medicamento ->EstoqueMedicamentos ->  EstoqueMedicamentos -- certifica de subtrair uma unidade do medicamento
certfy med (estoque:estoques)  | med == fst estoque = (med,snd estoque -1 ):estoques
                               | otherwise = [estoque] ++ (certfy med estoques)


{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med est | verifica med est 0 == False = 0
                             | otherwise = number med est

number :: Medicamento -> EstoqueMedicamentos -> Quantidade -- retorna a quantidade de medicamentos
number med (estoque:estoques) | med == fst estoque = snd estoque
                              | otherwise = number med estoques


{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos ((med,h):calda) = quickSort ([(med,length h)] ++ demandaMedicamentos calda) -- ordena 


quickSort :: Ord a => [a] -> [a] --funçao apresentada pelo professor/ ordena 
quickSort [] = []
quickSort (a:as) = quickSort [e | e <- as, e < a] ++ [a] ++ quickSort [e | e <- as, e >= a]


quickSort2 :: Ord a => [a] -> [a] --funçao apresentada pelo professor/ ordena e elimina repetições
quickSort2 [] = []
quickSort2 (a:as) = quickSort2 [e | e <- as, e < a] ++ [a] ++ quickSort2 [e | e <- as, e > a]


{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

receituarioValido :: Receituario -> Bool
receituarioValido ((med, h):calda) | doSort ((med, h):calda) == False = False
                                | ordenaest ((med,h):calda) == returnest ((med,h):calda) = True
                                | ordenaest ((med,h):calda) /= returnest ((med,h):calda) = False

horaordena :: [Horario] -> Bool -- verifica se a hora esta ordenada
horaordena [] = True
horaordena h |h == quickSort2 h = True
             |h /= quickSort2 h = False

doSort :: Receituario -> Bool -- percorre receituario afim de verificar os horarios 
doSort ((_, h):calda) | horaordena h == False = False
                        | calda == [] = True
                        |otherwise = doSort calda

ordenaest :: Receituario -> EstoqueMedicamentos -- retorna um estoque ordenado 
ordenaest [] = []
ordenaest ((med, h):calda) = quickSort2 ([(med,length h)] ++ ordenaest calda)

returnest :: Receituario -> EstoqueMedicamentos -- retorna o estoque 
returnest [] = []
returnest ((med,h):calda) = [(med,length h)] ++ (returnest calda)

planoValido :: PlanoMedicamento -> Bool -- todas as funções tem o mesmo pensamento 
planoValido  ((h,(med:meds)):calda) | doSort2  ((h,(med:meds)):calda) == False = False
                                | ordenaest2  ((h,(med:meds)):calda) == returnest2  ((h,(med:meds)):calda) = True
                                | ordenaest2  ((h,(med:meds)):calda) /= returnest2  ((h,(med:meds)):calda) = False

horaordena2 :: [Medicamento] -> Bool
horaordena2 [] = True
horaordena2 h |h == quickSort2 h = True
         |h /= quickSort2 h = False

doSort2 :: PlanoMedicamento -> Bool
doSort2 ((_,med):calda) | horaordena2 med == False = False
                        | calda == [] = True
                        |otherwise = doSort2 calda

ordenaest2 :: PlanoMedicamento -> PlanoMedicamento
ordenaest2 [] = []
ordenaest2 ((h,(med)):calda) = quickSort2 ( [(h,(med))] ++ ordenaest2 calda)

returnest2 :: PlanoMedicamento -> PlanoMedicamento
returnest2 [] = []
returnest2 ((h,(med)):calda) = [(h,(med))] ++ returnest2 calda


{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

plantaoValido :: Plantao -> Bool
plantaoValido ((h,c:cs):calda)  = master ((h,c:cs):calda)

master :: Plantao -> Bool -- função mestre 
master ((_,_):[]) = True
master ((h,c:cs):calda) | (hordem ((h,c:cs):calda) && traduzmed (c:cs) [] [] ) == False = False
master ((h,c:cs):calda) |(hordem ((h,c:cs):calda) && traduzmed (c:cs) [] [] )== True = master calda

hordem :: Plantao -> Bool -- verifica se as horas estão ordenadas 
hordem ((_,_):[]) = True
hordem ((h,_): (h2,c2) :calda)| h < h2  = hordem ((h2,c2):calda)
                         | h > h2  = False

traduzmed :: [Cuidado]-> [Medicamento] -> [Medicamento] -> Bool -- retira os medicamentos de cuidado e separa em vetores(medicar/comprar)
traduzmed [] v v2 | vMedComp v v2 [] && sort v v == True = True
             | otherwise  = False
traduzmed ((x):calda) v v2 = case x of
            Medicar c -> traduzmed calda (v++[c]) v2
            Comprar c _ -> traduzmed calda v (v2++[c])

vMedComp :: [Medicamento] -> [Medicamento] -> [Medicamento] -> Bool -- verifica se tem medicar(ex:m1) e comprar(ex:m1) no mesmo lugar 
vMedComp (_:ms) [] backup = vMedComp ms backup []
vMedComp [] _ _ = True
vMedComp (m:ms) (m1:ms1) backup | m == m1 = False
                 | m /= m1 = vMedComp (m:ms) ms1 ([m1]++backup)

sort :: [Medicamento] -> [Medicamento] -> Bool -- verifica se esta ordenado
sort a b | quickSort2 (a) == b = True
        | quickSort2 (a) /= b = False


{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario ((med,h:hs):calda) = correR ((med,h:hs):calda) []

correR :: Receituario -> PlanoMedicamento -> PlanoMedicamento -- corre receituario para retornar um plano
correR [] p = arruma (quickSort2 p)
correR ((_,[]):calda) p = correR calda p
correR ((med,h:hs):calda) p = correR ((med,hs):calda)  (quickSort2([(h,correH ((med,h:hs):calda) h [] )]++p) )

correH ::  Receituario-> Horario -> [Medicamento] ->  [Medicamento] -- retorna a lista de medicamentos de um determinado horario
correH [] _ b = quickSort2 (b)
correH ((_,[]):calda) h b = correH calda h b
correH ((med, (h1:hs1) ):calda) h b | h == h1 = correH calda h ([med] ++ b)
                            | h /= h1 = correH ((med,hs1):calda) h b
                            |otherwise = []

arruma :: PlanoMedicamento -> PlanoMedicamento -- retira repetições
arruma (x:[]) = [x]
arruma ((h,med:meds):(h2,med2:meds2):calda) | h == h2 = arruma ((h,med:meds):calda)
                                            | h /= h2 = [(h,med:meds)] ++ arruma ( ((h2,med2:meds2):calda))


{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

geraReceituarioPlano :: PlanoMedicamento -> Receituario --mesmo pensamento da 7
geraReceituarioPlano  ((h,med:meds):calda) = achaR ((h,med:meds):calda) []

achaR :: PlanoMedicamento -> Receituario -> Receituario -- corre plano para retornar receituario
achaR [] r = arruma2 (quickSort2 r)
achaR ((_,[]):calda) r = achaR calda r
achaR ((h,med:meds):calda) r = achaR ((h,meds):calda) (quickSort2([(med,achaH ((h,med:meds):calda) med [])]++r))

achaH :: PlanoMedicamento -> Medicamento -> [Horario]  -> [Horario] -- retorna a lista de horario de um determinado medicamento
achaH [] _ a =quickSort2 a
achaH ((_,[]):calda) med1 a = achaH calda med1 a
achaH ((h,med:meds):calda) med1 a | med1 == med = achaH calda med1 ([h] ++ a )
                                | med1 /= med = achaH ((h,meds):calda) med1 a

arruma2 :: Receituario -> Receituario -- retira repetições 
arruma2 (x:[]) = [x]
arruma2 ((h,med:meds):(h2,med2:meds2):calda) | h == h2 = arruma2 ((h,med:meds):calda)
                                            | h /= h2 = [(h,med:meds)] ++ arruma2 ( ((h2,med2:meds2):calda))


{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao ((h,c:cs):calda) est | passofinal (mestre ((h,c:cs):calda) est) (mestre ((h,c:cs):calda) est) == Nothing = Nothing
                                    | passofinal (mestre ((h,c:cs):calda) est) (mestre ((h,c:cs):calda) est) /= Nothing = Just (mestre ((h,c:cs):calda) est)

mestre :: Plantao -> EstoqueMedicamentos -> EstoqueMedicamentos -- função mestra 
mestre [] est = est
mestre ((_,c:cs):calda) est = mestre calda (contador (c:cs) est)

contador :: [Cuidado]-> EstoqueMedicamentos  ->  EstoqueMedicamentos  -- corre cuidado e retorna estoque 
contador []  ((med,qt):calda) = ((med,qt):calda)
contador ((x):calda) est  = case x of
              Medicar c ->contador calda (casomed c est 1)
              Comprar c n ->contador calda (casocomp c est n)

-- Tem um erro aqui, verificar o problema
casomed :: Medicamento -> EstoqueMedicamentos -> Quantidade ->  EstoqueMedicamentos -- (se for medicar) subtrai e verifica se o resultado é menor que 0
casomed med ((chave,chaves):caldaC) qt | med == chave && (chaves - qt) < 0 = [(med,-9999)]
                                | med == chave && (chaves - qt) >= 0 = ((chave,chaves-1):caldaC)
                                | otherwise =[(chave,chaves)] ++(casomed med caldaC qt)

casocomp :: Medicamento -> EstoqueMedicamentos -> Quantidade ->  EstoqueMedicamentos -- se for comprar ele adiciona a quantidade 
casocomp med ((chave,chaves):caldaC) qt | med == chave = ((chave,chaves+qt):caldaC)
                                | otherwise =[(chave,chaves)] ++(casocomp med caldaC qt)

passofinal :: EstoqueMedicamentos  ->  EstoqueMedicamentos -> Maybe EstoqueMedicamentos -- verifica se existe um -9999, se existir retorna nothing
passofinal [] est = Just est
passofinal ((_,chaves):caldaC) est | chaves == -9999 = Nothing  
                                | otherwise = passofinal caldaC est


{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz ((h,cuid):calda) ((h2,med):calda2) est
    | executaPlantao ((h,cuid):calda) est == Nothing = False
    | horacerta ((h,cuid):calda) ((h2,med):calda2)  == True && retiraMed ((h,cuid):calda) [] (medplano ((h2,med):calda2)) == True = True
    | horacerta ((h,cuid):calda) ((h2,med):calda2)  /= True && retiraMed ((h,cuid):calda) [] (medplano ((h2,med):calda2)) /= True = False

medplano :: PlanoMedicamento -> [Medicamento] -- retira todos os medicamentos do plano
medplano [] = []
medplano ((_,med):calda) =  (med ++ (medplano calda))


horacerta :: Plantao -> PlanoMedicamento -> Bool -- verifica se as horas estao corretas
horacerta a b | length a /= length b = False
horacerta [] [] = True 
horacerta ((h1,_):calda) ((h2,_):calda2) | h1 == h2 = horacerta calda calda2
                | otherwise =False 

retiraMed :: Plantao -> [Medicamento] -> [Medicamento] -> Bool -- retira os medicamentos do plantao (caso de medicar)
retiraMed [] (v:vs) b = vMedicamentos (v:vs) b   
retiraMed ((_,((x):_)):calda)  v v2 = case x of
            Medicar c -> retiraMed calda (v++[c]) v2
            Comprar _ _ -> retiraMed calda v v2

vMedicamentos :: [Medicamento] -> [Medicamento] -> Bool -- verifica se os dois vetores sao iguais
vMedicamentos a b | length a/= length b = False
vMedicamentos [] [] = True
vMedicamentos (v:vs) (v2:vs2) | v == v2 = vMedicamentos vs vs2
                     | otherwise = False


{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto plano est = addcomp (transforma plano est) est est

transforma :: PlanoMedicamento  ->  EstoqueMedicamentos -> Plantao -- transforma plano em plantao
transforma [] _ = []
transforma ((_,[]):calda) est = transforma calda  est
transforma ((h,med:meds):calda) est = [(h,([Medicar med]++[]))] ++ transforma ((h,meds):calda)  est

addcomp :: Plantao -> EstoqueMedicamentos -> EstoqueMedicamentos -> Plantao -- adiciona comprar onde for necessario 
addcomp [a] _ _ = [a]
addcomp ((_,_):(h2,med2):calda) [] b = addcomp ((h2,med2):calda) b b 
addcomp ((h,c):(h2,med2):calda) ((medica,q):resto) b
                        | returnMed med2 [] == [medica] && q /= 0 = [(h,c)] ++ addcomp ((h2,med2):calda) b b
                        | returnMed med2 [] == [medica] && q == 0 = [(h,c++[Comprar medica 10])] ++ addcomp ((h2,med2):calda) ((medica,q):resto) b
                        | returnMed med2 [] /= [medica] = addcomp ((h,c):(h2,med2):calda) resto b

returnMed :: [Cuidado] -> [Medicamento] -> [Medicamento] -- retira o medicamentos do cuidado (caso medicar)
returnMed [] b  = b  
returnMed ((x):calda)  v  = case x of
            Medicar c -> returnMed calda [c]
            Comprar _ _ -> returnMed calda v
