module Root.Typechecker.Typechecker where

import Root.Typechecker.AbsLOO
import Root.Typechecker.PrintLOO


data R a = OK a | Erro String
  deriving (Eq, Ord, Show, Read)

isError :: R a -> Bool
isError e = case e of
  OK _ -> False
  Erro _ -> True
 
data ConstAnnotation = Const | NotConst
  deriving (Eq, Show)

type TEnvironment = ([TContext], [ClassDeclaration])

type TContext = [(Ident, (Type, ConstAnnotation))]

initialEnvironment :: TEnvironment
initialEnvironment = ([], [])

typeCheckProgram :: Program -> [R TEnvironment]
typeCheckProgram (Prog cds) = case loadClassDeclarations initialEnvironment cds of
  OK env -> concatMap (typeCheckClass env) cds
  Erro msg -> [Erro msg]

typeCheckClass :: TEnvironment -> ClassDeclaration -> [R TEnvironment]
typeCheckClass env cd@(ClassD id ext members) = map (typeCheckMember env cd) members

typeCheckMember :: TEnvironment -> ClassDeclaration -> MemberDeclaration -> R TEnvironment
typeCheckMember env _ (Attr _) = OK env
typeCheckMember env cd m@(Mth retType id decls stms) = typeCheckMethod env cd m

typeCheckMethod :: TEnvironment -> ClassDeclaration -> MemberDeclaration -> R TEnvironment
typeCheckMethod _ _ (Attr _) = error "Chamada inválida de 'typeCheckMethod' com um atributo como parâmetro"
typeCheckMethod env cd m@(Mth retType id decls stms) =
  if deadCodeFree stms
    then
      if retType /= Tvoid
        then
          if checkExecutionPathExhaustion stms
            then processBindingsAndTypeCheckMethod env cd m
            else Erro "Nem todo caminho de execucao tem comando return."
        else processBindingsAndTypeCheckMethod env cd m
    else Erro "Ha deadcode no programa!"

processBindingsAndTypeCheckMethod :: TEnvironment -> ClassDeclaration -> MemberDeclaration -> R TEnvironment
processBindingsAndTypeCheckMethod _ _ (Attr _) = error "Chamada inválida de 'processBindingsAndTypeCheckMethod' com um atributo como parâmetro"
processBindingsAndTypeCheckMethod environment@(sc, classCtx) cd@(ClassD cid _ ms) (Mth tR id decls stms) = case lookupClassDecl classCtx cid of
  OK cd'@(ClassD cid' _ ms') -> tk newEnvironment (SBlock stms) tR
    where
      paramBindings = map (\(Dec tp id) -> (id, (tp, Const))) decls
      classBindings = map (\(Attr (Dec tp id)) -> (id, (tp, NotConst))) $ filter isAttribute ms'
      thisBinding = (Ident "this", (TClass cid, NotConst))
      isAttribute (Attr _) = True
      isAttribute _ = False
      newEnvironment = pushB (classBindings ++ paramBindings ++ [thisBinding]) environment
  Erro msg -> Erro msg

checkExecutionPathExhaustion :: [Stm] -> Bool
checkExecutionPathExhaustion [] = False
checkExecutionPathExhaustion (s : stms) =
  if checkExecutionPathExhaustionS s
    then null stms
    else checkExecutionPathExhaustion stms

checkExecutionPathExhaustionS :: Stm -> Bool
checkExecutionPathExhaustionS x = case x of
  SReturn exp -> True
  SAss id exp -> False
  SExp exp -> False
  SDec (Dec tp id) -> False
  SConstInit (Dec tp id) exp -> False
  SBlock stms -> checkExecutionPathExhaustion stms
  SWhile exp stm -> checkExecutionPathExhaustionS stm
  SIf exp stmT stmE ->
    checkExecutionPathExhaustionS stmT
      && checkExecutionPathExhaustionS stmE

deadCodeFree :: [Stm] -> Bool
deadCodeFree [] = True
deadCodeFree (s : stms) =
  deadCodeFreeS s
    && ( if checkExecutionPathExhaustionS s
           then null stms
           else deadCodeFree stms
       )

deadCodeFreeS :: Stm -> Bool
deadCodeFreeS x = case x of
  SReturn exp -> True
  SExp exp -> True
  SAss id exp -> True
  SDec (Dec tp id) -> True
  SConstInit (Dec tp id) exp -> True
  SBlock stms -> deadCodeFree stms
  SWhile exp stm -> deadCodeFreeS stm
  SIf exp stmT stmE -> deadCodeFreeS stmT && deadCodeFreeS stmE

-- DICA: modificando-se uma linha da função abaixo, resolve-se uma das questões
tk :: TEnvironment -> Stm -> Type -> R TEnvironment
tk environment@(sc, classCtx) x tR = case x of
  SConstInit (Dec tp id) exp -> case tke environment exp tp of
    OK _ -> updateShallowTypeA environment id (tp, Const)
    Erro msg -> Erro msg
  SDec (Dec tp id) -> updateShallowTypeA environment id (tp, NotConst)
  SReturn exp -> tke environment exp tR
  sAss@(SAss id exp) ->
    if lookupDeepConstAnnotation environment id /= Const
      then
        let r = tinf environment exp
         in case r of
              OK (TClass cid) ->
                case lookupDeepType environment id of
                  OK (TClass cid') -> case (lookupClassDecl classCtx cid', lookupClassDecl classCtx cid) of
                    (OK cd1, OK cd2) -> if isSubClass classCtx cd2 cd1 then OK environment else Erro $ "Erro em atribuição: a classe '" ++ show cid ++ "' deveria ser subclasse da classe '" ++ show id ++ "'"
                    (_, _) -> Erro $ "Erro ao verificar statement: classes fora do contexto: '" ++ show id ++ "' e '" ++ show cid ++ "'"
                  _ -> Erro $ "O tipo de '" ++ show id ++ "' deveria ser uma classe."
              OK x ->
                let r2 = tke environment (EVar id) x
                 in case r2 of
                      OK env -> OK env
                      Erro msg -> Erro (msg ++ " no comando: " ++ printTree sAss)
              Erro msg -> Erro (msg ++ " no comando: " ++ printTree sAss)
      else OK environment
  SBlock [] -> OK environment
  SBlock (sConstInit@(SConstInit (Dec tp id) exp) : stms) ->
    case tk environment sConstInit tp of
      OK env -> tk env (SBlock stms) tR
      Erro msg -> Erro (msg ++ " no comando: " ++ printTree sConstInit)
  SBlock (sDec@(SDec (Dec tp id)) : stms) ->
    let r = updateShallowType environment id tp
     in case r of
          OK env -> tk env (SBlock stms) tR
          Erro msg -> Erro (msg ++ " no comando: " ++ printTree sDec)
  SBlock (sb@(SBlock bls) : stms) ->
    let r = tk (push environment) sb tR
     in case r of
          OK _ -> tk (pop environment) (SBlock stms) tR
          Erro msg -> Erro (msg ++ " no comando: " ++ printTree sb)
  SBlock (s : stms) ->
    let r = tk environment s tR
     in case r of
          OK env -> tk env (SBlock stms) tR
          Erro msg -> Erro msg
  sWhile@(SWhile exp stm) ->
    let r = tke environment exp Tint
     in case r of
          OK _ -> tk environment stm tR
          Erro msg -> Erro (msg ++ " no comando: " ++ printTree sWhile)
  SExp exp -> case tinf environment exp of
    OK _ -> OK environment
    Erro msg -> Erro msg
  sIf@(SIf exp stmT stmE) ->
    let r = tke environment exp Tint
     in case r of
          OK _ ->
            let r2 = tk environment stmT tR
             in case r2 of
                  OK _ -> tk environment stmE tR
                  Erro msg -> Erro (msg ++ " no comando: " ++ printTree sIf)
          Erro msg -> Erro (msg ++ " no comando: " ++ printTree sIf)

-- DICA: modificando-se a uma linha da função abaixo, resolve-se uma das questões
tke :: TEnvironment -> Exp -> Type -> R TEnvironment
tke environment@(sc, classCtx) exp tp =
  let r = tinf environment exp
   in case r of
        OK (TClass eid) -> case tp of
          TClass tid -> case (lookupClassDecl classCtx eid, lookupClassDecl classCtx tid) of
            (OK ecid, OK tcid) -> OK environment
            (_, _) -> Erro "Erro ao verificar expressão: classes fora do contexto."
          _ -> Erro $ "Erro em verificação de expressão: o tipo '" ++ show r ++ "' deveria ser um subtipo de '" ++ show tp ++ "'"
        OK tipo ->
          if tipo == tp
            then OK environment
            else
              Erro
                ( " @typechecker:  A expressao " ++ printTree exp ++ " tem tipo " ++ printTree tipo
                    ++ " mas o tipo esperado eh "
                    ++ printTree tp
                )
        Erro msg -> Erro msg

combChecks :: TEnvironment -> Exp -> Exp -> Type -> R Type
combChecks environment exp1 exp2 tp =
  let r = tke environment exp1 tp
   in case r of
        OK _ ->
          let r2 = tke environment exp2 tp
           in case r2 of
                OK _ -> OK tp
                Erro msg -> Erro msg
        Erro msg -> Erro msg

-- DICA: na função abaixo, há duas linhas que podem ser modificadas para resolver duas das questões
tinf :: TEnvironment -> Exp -> R Type
tinf environment@(_, classCtx) x = case x of
  ECon exp0 exp -> combChecks environment exp0 exp TStr
  EAdd exp0 exp -> combChecks environment exp0 exp Tint
  ESub exp0 exp -> combChecks environment exp0 exp Tint
  EMul exp0 exp -> combChecks environment exp0 exp Tint
  EDiv exp0 exp -> combChecks environment exp0 exp Tint
  EOr exp0 exp -> combChecks environment exp0 exp Tbool
  EAnd exp0 exp -> combChecks environment exp0 exp Tbool
  ENot exp ->
    let r = tke environment exp Tbool
     in case r of
          OK _ -> OK Tbool
          Erro msg -> Erro msg
  EStr str -> OK TStr
  ETrue -> OK Tbool
  EFalse -> OK Tbool
  EInt n -> OK Tint
  EVar id -> lookupDeepType environment id
  EMthCall lid rid args -> case lookupDeepType environment lid of
    OK (TClass cid) -> case lookupClassDecl classCtx cid of
      OK cd -> case getMethodInfo rid cd of
        OK (retType, decls, ms) ->
          if length decls == length args
            then
              if isThereError tksArgs /= []
                then Erro " @typechecker: chamada de funcao invalida"
                else OK retType
            else Erro " @typechecker: tamanhos diferentes de lista de argumentos e parametros"
          where
            parameterTypes = map (\(Dec tp _) -> tp) decls
            tksArgs = zipWith (tke environment) args parameterTypes
            isThereError l =
              filter
                (== False)
                ( map
                    ( \e ->
                        ( let r2 = e
                           in case r2 of
                                OK _ -> True
                                Erro _ -> False
                        )
                    )
                    l
                )
        Erro msg -> OK Tvoid
      Erro msg -> Erro $ "@typechecker: Erro ao obter informações da classe'" ++ show cid ++ "': " ++ msg
    OK _ -> Erro $ "@typechecker: Não é possível invocar métodos da variável'" ++ show lid ++ "', pois esta não tem o tipo de uma classe"
    Erro msg -> Erro msg
  ECast id exp -> case (lookupClassDecl classCtx id, tinf environment exp) of
    (OK cd@(ClassD cid _ _), OK tp) -> OK $ TClass cid
    _ -> Erro "@typechecker: cast invalido."
  ENew id -> case lookupClassDecl classCtx id of
    OK _ -> OK $ TClass id
    Erro msg -> Erro msg

compatibleCast :: TEnvironment -> ClassDeclaration -> Exp -> R Bool
compatibleCast env@(sc, classCtx) castCd exp = case tinf env exp of
  OK (TClass expCid) -> case lookupClassDecl classCtx expCid of
    OK expCd -> OK $ isSuperClass classCtx expCd castCd || isSubClass classCtx expCd castCd
    _ -> Erro $ "@typechecker: cast invalido: erro ao encontrar a definição da classe '" ++ show expCid ++ "'."
  _ -> Erro $ "@typechecker: o tipo da expressão '" ++ show exp ++ "' deve ser o de uma classe."

loadClassDeclarations :: TEnvironment -> [ClassDeclaration] -> R TEnvironment
loadClassDeclarations e [] = OK e
loadClassDeclarations (sc, classCtx) (cd@(ClassD id members ext) : cds) =
  if any (\cd -> hasCycleOnInheritanceHierarchy classCtx cd /= OK False) classCtx
    then Erro "Erro na hierarquia de herança: há ciclos nas heranças entre as classes!"
    else
      ( case constructValorClassDeclaration classCtx cd of
          Erro msg -> Erro $ "Erro ao carregar as declarações da classe '" ++ show id ++ "' :" ++ msg
          OK decls -> case updateClassCtx classCtx decls of
            OK newClassContext -> loadClassDeclarations (sc, newClassContext) cds
            Erro msg -> Erro $ "Erro ao carregar as declarações das classes: " ++ msg
      )

constructValorClassDeclaration :: [ClassDeclaration] -> ClassDeclaration -> R ClassDeclaration
constructValorClassDeclaration e cd@(ClassD id ext ms) = case constructMemberList e cd of
  OK overridenMs -> OK $ ClassD id ext overridenMs
  Erro msg -> Erro $ "Error no processamento da classe '" ++ show id ++ "' :" ++ msg
  where
    constructMemberList _ (ClassD _ ExtObject ms) = OK ms
    constructMemberList e (ClassD _ (ExtId parentId) ms) = case lookupClassDecl e parentId of
      OK parentCd -> case constructMemberList e parentCd of
        OK parentMs -> OK $ overrideMembers ms parentMs
        Erro msg -> Erro $ "Error na herança da classe '" ++ show parentId ++ "' :" ++ msg
      Erro msg -> Erro $ "Error na herança da classe '" ++ show parentId ++ "' :" ++ msg
    overrideMembers [] inheritedMembers = inheritedMembers
    overrideMembers (newMember : newMembers) inheritedMembers =
      overrideMembers newMembers $
        if newMember `elem` inheritedMembers
          then map (\im -> if im == newMember then newMember else im) inheritedMembers
          else newMember : inheritedMembers

lookupClassDecl :: [ClassDeclaration] -> Ident -> R ClassDeclaration
lookupClassDecl [] lid = Erro $ "Classe não encontrada: '" ++ show lid ++ "'"
lookupClassDecl (cd@(ClassD id _ _) : cds) lid = if id == lid then OK cd else lookupClassDecl cds lid

updateClassCtx :: [ClassDeclaration] -> ClassDeclaration -> R [ClassDeclaration]
updateClassCtx cds ucd@(ClassD uid _ _) = if any (\cd@(ClassD id _ _) -> id == uid) cds then Erro $ "Redeclaração da classe '" ++ show uid ++ "'" else OK (ucd : cds)

pushB :: TContext -> TEnvironment -> TEnvironment
pushB typeBindings (sc, classCtx) = (typeBindings : sc, classCtx)

push :: TEnvironment -> TEnvironment
push (sc, classCtx) = ([] : sc, classCtx)

pop :: TEnvironment -> TEnvironment
pop (s : scs, classCtx) = (scs, classCtx)
pop ([], _) = error "Error on 'pop' call: no context to pop from environment"

lookupDeepConstAnnotation :: TEnvironment -> Ident -> ConstAnnotation
lookupDeepConstAnnotation ([], _) id = error $ "Error on 'lookupDeepConstAnnotation' call: no id '" ++ show id ++ "'  on environment"
lookupDeepConstAnnotation e@(s : scs, classCtx) id =
  let r = lookupShallow s id
   in case r of
        OK (_, a) -> a
        Erro _ -> lookupDeepConstAnnotation (scs, classCtx) id

lookupDeepType :: TEnvironment -> Ident -> R Type
lookupDeepType ([], classCtx) id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookupDeepType (s : scs, classCtx) id =
  let r = lookupShallow s id
   in case r of
        OK (tp, _) -> OK tp
        Erro _ -> lookupDeepType (scs, classCtx) id

lookupShallow :: TContext -> Ident -> R (Type, ConstAnnotation)
lookupShallow [] s = Erro ("@typechecker: " ++ printTree s ++ " nao esta no contexto. ")
lookupShallow ((i, v) : cs) s
  | i == s = OK v
  | otherwise = lookupShallow cs s

updateShallowTypeA :: TEnvironment -> Ident -> (Type, ConstAnnotation) -> R TEnvironment
updateShallowTypeA ([], classCtx) id (tp, a) = OK ([[(id, (tp, a))]], classCtx)
updateShallowTypeA (s : sc, classCtx) id (tp, a) =
  let r = updateShallowA s id (tp, a)
   in case r of
        OK cxt -> OK (cxt : sc, classCtx)
        Erro msg -> Erro msg

updateShallowA :: TContext -> Ident -> (Type, ConstAnnotation) -> R TContext
updateShallowA context id (tp, a) =
  if id `elem` map fst context
    then Erro "@typechecker: tipo ja definido no contexto de tipos"
    else OK ((id, (tp, a)) : context)

updateShallowType :: TEnvironment -> Ident -> Type -> R TEnvironment
updateShallowType ([], classCtx) id tp = OK ([[(id, (tp, NotConst))]], classCtx)
updateShallowType (s : sc, classCtx) id tp =
  let r = updateShallowA s id (tp, NotConst)
   in case r of
        OK cxt -> OK (cxt : sc, classCtx)
        Erro msg -> Erro msg

getMethodInfo :: Ident -> ClassDeclaration -> R (Type, [Decl], [Stm])
getMethodInfo method (ClassD cn _ []) = Erro $ "Error on 'getMethodInfo': no method '" ++ show method ++ "' defined on class '" ++ show cn ++ "'"
getMethodInfo method (ClassD cn ext ((Mth tp mth decls stms) : ms)) =
  if method == mth
    then OK (tp, decls, stms)
    else getMethodInfo method (ClassD cn ext ms)
getMethodInfo method (ClassD cn ext ((Attr _ : ms))) = getMethodInfo method (ClassD cn ext ms)

hasCycleOnInheritanceHierarchy :: [ClassDeclaration] -> ClassDeclaration -> R Bool
hasCycleOnInheritanceHierarchy classCtx cd@(ClassD id ext ms) = hasCycleOnInheritanceHierarchyAux classCtx cd [id]
  where
    hasCycleOnInheritanceHierarchyAux _ (ClassD _ ExtObject _) _ = OK False
    hasCycleOnInheritanceHierarchyAux classCtx (ClassD cid (ExtId parentId) ms) vs = case lookupClassDecl classCtx parentId of
      OK cd'@(ClassD cid' ext' ms') -> if cid' `elem` vs then OK True else hasCycleOnInheritanceHierarchyAux classCtx cd' (cid' : vs)
      Erro msg -> Erro msg

isSuperClass :: [ClassDeclaration] -> ClassDeclaration -> ClassDeclaration -> Bool
isSuperClass classCtx cd1@(ClassD cid1 ext1 ms1) cd2@(ClassD cid2 ExtObject ms2) = cid1 == cid2
isSuperClass classCtx cd1@(ClassD cid1 ext1 ms1) cd2@(ClassD cid2 (ExtId pid2) ms2) =
  (cid1 == cid2)
    || ( case lookupClassDecl classCtx pid2 of
           OK pcd2 -> isSuperClass classCtx cd1 pcd2
           Erro s -> error "Erro no contexto de classes"
       )

isSubClass :: [ClassDeclaration] -> ClassDeclaration -> ClassDeclaration -> Bool
isSubClass classCtx cd1 cd2 = isSuperClass classCtx cd2 cd1