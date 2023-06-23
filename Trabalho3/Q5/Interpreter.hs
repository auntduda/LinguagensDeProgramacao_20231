module Root.Questoes.Q5.Interpreter where

import Root.Questoes.Q5.AbsLI
import Prelude hiding (lookup)

-- As definições deste arquivo são as mínimas para compilar os testes.
-- Você deverá completar todo o restante do código.
-- Dica: se você fez os exercícios anteriores, boa parte do código
-- pode ser reutilizado neste exercício.

type RContext = [(String, Valor)]

type ErrorMessage = String

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
    SAss id exp -> case eval context exp of
                    Right v -> Right (update context (getStr id) v)
                    Left msg -> Left msg
    SBlock [] -> Right (context)
    SBlock (s:stms) -> case execute context s of
                        Right v -> execute v (SBlock stms)
                        Left msg -> Left msg
    SWhile exp stm -> case eval context exp of
                      Right v -> if ((i v) /= 0) then case execute context stm of
                                                    Right v1 -> execute v1 (SWhile exp stm)
                                                    Left msg -> Left msg
                                  else Right context
                      Left msg -> Left msg
--     SdoWhile stm exp -> execute (execute context stm) (SWhile exp stm)
    SdoWhile stm exp -> case execute context stm of
                        Right v -> execute v (SWhile exp stm)
                        Left msg -> Left msg
    STry stmsT stmsC stmsF -> case stmsT of
        [] -> execute context (SBlock stmsF)
        (s:ss) -> case execute context s of
                        Right c -> execute c (STry ss stmsC stmsF)
                        Left msg -> case execute context (SBlock stmsC) of
                                        Right m -> execute m (SBlock stmsF)
                                        Left msg -> Left msg

eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
  EAdd e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> Right (ValorInt ((i ve1) + (i ve2)))
                                Left msg -> Left msg
                  Left msg -> Left msg
  ESub e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> Right (ValorInt ((i ve1) - (i ve2)))
                                Left msg -> Left msg
                  Left msg -> Left msg
  EMul e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> Right (ValorInt ((i ve1) * (i ve2)))
                                Left msg -> Left msg
                  Left msg -> Left msg
  EVar id -> Right (lookup context (getStr id)) 
  EDiv e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> if ((i ve2) == 0) then Left ("divisao por 0")
                                             else Right (ValorInt ((i ve1) `div` (i ve2)))
                                Left msg -> Left msg
                  Left msg -> Left msg
  EInt n ->  Right (ValorInt n)
  ECon exp0 exp -> case eval context exp0 of
                   Right v -> case eval context exp of
                              Right v1 -> Right (ValorStr (s v ++ s v1))
                              Left msg -> Left msg
                   Left msg -> Left msg
  EStr str -> Right (ValorStr str)
  EOr exp0 exp -> case eval context exp0 of
                  Right v -> case eval context exp of
                              Right v1 -> Right (ValorBool (b v || b v1))
                              Left msg -> Left msg
                  Left msg -> Left msg
  EAnd exp0 exp -> case eval context exp0 of
                  Right v -> case eval context exp of
                              Right v1 -> Right (ValorBool (b v && b v1))
                              Left msg -> Left msg
                  Left msg -> Left msg
  ENot exp -> case eval context exp of
                  Right v1 -> Right (ValorBool (not (b v1)))
                  Left msg -> Left msg
  ETrue -> Right (ValorBool True)
  EFalse -> Right (ValorBool False)
  
getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv