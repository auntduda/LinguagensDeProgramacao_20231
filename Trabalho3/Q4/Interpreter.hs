module Root.Questoes.Q4.Interpreter where

import Root.Questoes.Q4.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]
type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar (conforme abaixo),
   mas a sua definicao (corpo) pode continuar a mesma dos exercícios anteriores
-}
executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
-- executeP = undefined 

{- Dica: o tipo de execute deve mudar para
 execute :: RContext -> Stm -> Either ErrorMessage RContext
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos
 serao afetados
-}
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
                      Right v -> if (v /= 0) then case execute context stm of
                                                    Right v1 -> execute v1 (SWhile exp stm)
                                                    Left msg -> Left msg
                                  else Right context
                      Left msg -> Left msg
    STry stmsT stmsC stmsF -> case stmsT of
        [] -> execute context (SBlock stmsF)
        (s:ss) -> case execute context s of
                        Right c -> execute c (STry ss stmsC stmsF)
                        Left msg -> case execute context (SBlock stmsC) of
                                        Right m -> execute m (SBlock stmsF)
                                        Left msg -> Left msg

{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
  EAdd e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> Right (ve1 + ve2)
                                Left msg -> Left msg
                  Left msg -> Left msg
  ESub e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> Right (ve1 - ve2)
                                Left msg -> Left msg
                  Left msg -> Left msg
  EMul e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> Right (ve1 * ve2)
                                Left msg -> Left msg
                  Left msg -> Left msg
  EVar id -> Right (lookup context (getStr id)) 
  
  -- algumas dicas abaixo...para voce adaptar o codigo acima
  
  EDiv e1 e2 -> case eval context e1 of
                  Right ve1 -> case eval context e2 of
                                Right ve2 -> if (ve2 == 0) then Left ("divisao por 0 na expressao: " ++ show (EDiv e1 e2))
                                             else Right (ve1 `div` ve2)
                                Left msg -> Left msg
                  Left msg -> Left msg
  EInt n ->  Right n


-- Dica: voce nao precisa mudar o codigo a partir daqui

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv