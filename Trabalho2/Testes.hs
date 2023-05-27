module Root.Testes.Testes where

import Data.Maybe (isNothing)
import Trabalho2-LP
import ModeloDados
import Test.Hspec

--------------------- DECLARAÇÕES ---------------------
med1 :: Medicamento
med1 = "Adera"

med2 :: Medicamento
med2 = "Alprazolam"

med3 :: Medicamento
med3 = "Donepezila"

med4 :: Medicamento
med4 = "Lactulona"

med5 :: Medicamento
med5 = "Mirtazapina"

med6 :: Medicamento
med6 = "Pantoprazol"

med7 :: Medicamento
med7 = "Patz"

med8 :: Medicamento
med8 = "Quetiapina"

med9 :: Medicamento
med9 = "Xarelto"

estoque1 :: EstoqueMedicamentos
estoque1 = [(med4, 10), (med6, 5), (med7, 0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(med4, 10), (med6, 5), (med7, 10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(med4, 10), (med6, 50), (med7, 10), (med8, 20)]

receituario1 :: Receituario
receituario1 = [(med4, [8, 17]), (med6, [6]), (med7, [22])]

receituario2 :: Receituario
receituario2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 22, 23])]

receituarioInvalido1 :: Receituario
-- Invalido: horário não está ordenado de forma crescente
receituarioInvalido1 = [(med4, [22, 10])]

receituarioInvalido2 :: Receituario
-- Invalido: medicamentos não estão ordenbados de forma crescente
receituarioInvalido2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med2, [8, 22, 23])]

receituarioInvalido3 :: Receituario
-- Invalido: medicamentos não estão ordenbados de forma crescente
receituarioInvalido3 = [(med4, [8, 17]), (med3, [6]), (med7, [22])]

receituarioInvalido4 :: Receituario
-- Invalido: horário não estão ordenbados de forma crescente
receituarioInvalido4 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 23, 22])]

plano1 :: PlanoMedicamento
plano1 = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

plano2 :: PlanoMedicamento
plano2 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med7, med8]), (23, [med8])] :: [(Int, [String])]

planoInvalido1 :: PlanoMedicamento
-- Invalido: horário não está ordenado de forma crescente
planoInvalido1 = [(8, [med4]), (6, [med6]), (17, [med4]), (22, [med7])]

planoInvalido2 :: PlanoMedicamento
-- Invalido: medicamentos não estão ordenbados de forma crescente
planoInvalido2 = [(6, [med6]), (8, [med8, med4]), (17, [med4]), (22, [med7, med8]), (23, [med8])] :: [(Int, [String])]

planoInvalido3 :: PlanoMedicamento
-- Invalido: medicamentos não estão ordenbados de forma crescente
planoInvalido3 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med8, med7]), (23, [med8])] :: [(Int, [String])]

planoInvalido4 :: PlanoMedicamento
-- Invalido: horário não está ordenado de forma crescente
planoInvalido4 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (23, [med8]), (22, [med7, med8])] :: [(Int, [String])]

plantao1 :: Plantao
plantao1 =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4]),
    (22, [Medicar med7])
  ]

plantao2 :: Plantao
plantao2 =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantao3 :: Plantao
plantao3 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido1 :: Plantao
plantaoInvalido1 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    (22, [Medicar med7]),
    -- Invalido: não está em horario crescente
    (17, [Medicar med4, Comprar med7 30])
  ]

plantaoInvalido2 :: Plantao
plantaoInvalido2 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    -- Invalido: comprar o mesmo medicamento no horário
    (17, [Medicar med4, Comprar med4 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido3 :: Plantao
plantaoInvalido3 =
  [ (6, [Medicar med6, Medicar med9]),
    -- Invalido: ordem dos medicamentos não está ordenada
    (8, [Medicar med4, Medicar med2]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido4 :: Plantao
plantaoInvalido4 =
  [ (6, [Medicar med6]),
    -- Invalido: comprar o mesmo medicamento no horário
    (8, [Comprar med4 20, Medicar med4, Medicar med8]),
    (17, [Medicar med4]),
    (22, [Medicar med7, Medicar med8]),
    (23, [Medicar med8])
  ]

--------------------- EXERCÍCIO 1 ---------------------
caso1_1 = comprarMedicamento med7 30 estoque1 == [(med4, 10), (med6, 5), (med7, 30)]

caso1_2 = comprarMedicamento med1 20 estoque1 == [(med1, 20), (med4, 10), (med6, 5), (med7, 0)]

caso1_3 = comprarMedicamento med6 2 estoque1 == [(med4, 10), (med6, 7), (med7, 0)]

caso1_4 = comprarMedicamento med9 20 [] == [(med9, 20)]

testeExercicio1 = and [caso1_1, caso1_2, caso1_3, caso1_4]

--------------------- EXERCÍCIO 2 ---------------------
caso2_1 = tomarMedicamento med4 estoque1 == Just [(med4, 9), (med6, 5), (med7, 0)]

caso2_2 = isNothing (tomarMedicamento med8 estoque1)

testeExercicio2 = and [caso2_1, caso2_2]

--------------------- EXERCÍCIO 3 ---------------------
caso3_1 = consultarMedicamento med6 estoque1 == 5

caso3_2 = consultarMedicamento "Aas" estoque1 == 0

testeExercicio3 = and [caso3_1, caso3_2]

--------------------- EXERCÍCIO 4 ---------------------
caso4_1 = demandaMedicamentos receituario1 == [(med4, 2), (med6, 1), (med7, 1)]

caso4_2 = demandaMedicamentos receituario2 == [(med4, 2), (med6, 1), (med7, 1), (med8, 3)]

testeExercicio4 = and [caso4_1, caso4_2]

--------------------- EXERCÍCIO 5a ---------------------
caso5_1a = receituarioValido receituario1

caso5_2a = receituarioValido receituario2

caso5_3a = not $ receituarioValido receituarioInvalido1

caso5_4a = not $ receituarioValido receituarioInvalido2

caso5_5a = not $ receituarioValido receituarioInvalido3

caso5_6a = not $ receituarioValido receituarioInvalido4

--------------------- EXERCÍCIO 5b ---------------------
testeExercicio5a = and [caso5_1a, caso5_2a, caso5_3a, caso5_4a, caso5_5a, caso5_6a]

caso5_1b = planoValido plano1

caso5_2b = planoValido plano1

caso5_3b = not $ planoValido planoInvalido1

caso5_4b = not $ planoValido planoInvalido2

caso5_5b = not $ planoValido planoInvalido3

caso5_6b = not $ planoValido planoInvalido4

testeExercicio5b = and [caso5_1b, caso5_2b, caso5_3b, caso5_4b, caso5_5b, caso5_6b]

--------------------- EXERCÍCIO 6 ---------------------
caso6_1 = plantaoValido plantao1

caso6_2 = plantaoValido plantao2

caso6_3 = plantaoValido plantao3

caso6_4 = not (plantaoValido plantaoInvalido1)

caso6_5 = not (plantaoValido plantaoInvalido2)

caso6_6 = not (plantaoValido plantaoInvalido3)

caso6_7 = not (plantaoValido plantaoInvalido4)

testeExercicio6 = and [caso6_1, caso6_2, caso6_3, caso6_4, caso6_5, caso6_6, caso6_7]

--------------------- EXERCÍCIO 7 ---------------------
caso7_1 = geraPlanoReceituario receituario1 == [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

caso7_2 = geraPlanoReceituario receituario2 == [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med7, med8]), (23, [med8])]

testeExercicio7 = and [caso7_1, caso7_2]

--------------------- EXERCÍCIO 8 ---------------------
caso8_1 = geraReceituarioPlano (geraPlanoReceituario receituario1) == receituario1

caso8_2 = geraReceituarioPlano (geraPlanoReceituario receituario2) == receituario2

testeExercicio8 = and [caso8_1, caso8_2]

--------------------- EXERCÍCIO 9 ---------------------
caso9_1 = isNothing (executaPlantao plantao1 estoque1)

caso9_2 = executaPlantao plantao1 estoque2 == Just [(med4, 8), (med6, 4), (med7, 9)]

caso9_3 = executaPlantao plantao2 estoque1 == Just [(med4, 8), (med6, 4), (med7, 29)]

testeExercicio9 = and [caso9_1, caso9_2, caso9_3]

--------------------- EXERCÍCIO 10 ---------------------
caso10_1 = not (satisfaz plantao1 plano1 estoque1)

caso10_2 = satisfaz plantao1 plano1 estoque2

caso10_3 = satisfaz plantao2 plano1 estoque1

testeExercicio10 = and [caso10_1, caso10_2, caso10_3]

--------------------- EXERCÍCIO 11 ---------------------
caso11_1 = satisfaz plantao plano1 estoque1
  where
    plantao = plantaoCorreto plano1 estoque1

caso11_2 = satisfaz plantao plano2 estoque2
  where
    plantao = plantaoCorreto plano2 estoque2

testeExercicio11 = and [caso11_1, caso11_2]

--------------------- VERIFICAÇÃO DOS TESTES ---------------------
main = hspec $ do
  describe "Suite de Testes do Trabalho 2" $ do
    it "O resultado global dos testes do exercício 1 deve 'True'" $ do
      testeExercicio1 `shouldBe` True

    it "O resultado global dos testes do exercício 2 deve 'True'" $ do
      testeExercicio2 `shouldBe` True

    it "O resultado global dos testes do exercício 3 deve 'True'" $ do
      testeExercicio3 `shouldBe` True

    it "O resultado global dos testes do exercício 4 deve 'True'" $ do
      testeExercicio4 `shouldBe` True

    it "O resultado global dos testes do exercício 5.1 deve 'True'" $ do
      testeExercicio5a `shouldBe` True
    it "O resultado global dos testes do exercício 5.2 deve 'True'" $ do
      testeExercicio5b `shouldBe` True

    it "O resultado global dos testes do exercício 6 deve 'True'" $ do
      testeExercicio6 `shouldBe` True

    it "O resultado global dos testes do exercício 7 deve 'True'" $ do
      testeExercicio7 `shouldBe` True

    it "O resultado global dos testes do exercício 8 deve 'True'" $ do
      testeExercicio8 `shouldBe` True

    it "O resultado global dos testes do exercício 9 deve 'True'" $ do
      testeExercicio9 `shouldBe` True

    it "O resultado global dos testes do exercício 10 deve 'True'" $ do
      testeExercicio10 `shouldBe` True

    it "O resultado global dos testes do exercício 11 deve 'True'" $ do
      testeExercicio11 `shouldBe` True