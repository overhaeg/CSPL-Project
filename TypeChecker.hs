module TypeChecker where
import           Parser
import qualified Data.Map.Strict as Map
import           Data.Either.Combinators

-- Environment 
type Env = Map.Map String (Either Type Kind)

addVariable :: Env -> String -> Either Type Kind -> Env 
addVariable env var typ = 
	Map.insert var typ env
addVar :: Env -> String -> Type -> Env
addVar env var typ =
    addVariable env var (Left typ)
addTVar :: Env -> String -> Kind -> Env
addTVar env tvar kind =
    addVariable env tvar (Right kind)

getVariableType :: Env -> String -> Maybe (Either Type Kind)
getVariableType env var = 
   Map.lookup var env
getVarType :: Env -> String -> Type
getVarType env var = 
    case getVariableType env var of
        (Just (Left v)) -> v
        _               -> error $ "Unknown Var " ++ var
getTVarKind :: Env -> String -> Kind
getTVarKind env tvar =
    case getVariableType env tvar of
        (Just (Right k)) -> k
        _                -> error $ "Unknown Type Variable " ++ tvar


-- Typing
checkType :: Env -> Exp -> Type
checkType _ (ValBool _) = TypeBool
checkType _ (ValNat _) = TypeNat
checkType env (ExpIsZero exp)
    | eqType typ TypeNat = TypeBool
    | otherwise = error $ "Invalid type (iszero)"
 where typ = checkType env exp
checkType env (ExpSucc exp)
    | eqType typ TypeNat = TypeNat
    | otherwise = error $ "Invalid type (succ)"
 where typ = checkType env exp
checkType env (ExpPred exp)
    | eqType typ TypeNat = TypeNat
    | otherwise = error $ "Invalid type (pred)"
 where typ = checkType env exp
checkType env (ExpIf cond frst sec) 
    | eqType typcond TypeBool && eqType typfst typsec = typfst
    | otherwise = error $ "Invalid type (If)"
 where typcond = checkType env cond
       typfst = checkType env frst
       typsec = checkType env sec
checkType env (ExpPlus frst sec)
    | eqType typfst TypeNat && eqType typscd TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Plus)"
 where typfst = checkType env frst
       typscd = checkType env sec
checkType env (ExpMin frst sec)
    | eqType typfst TypeNat && eqType typscd TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Min)"
 where typfst = checkType env frst
       typscd = checkType env sec
checkType env (ExpMult frst sec)
    | eqType typfst TypeNat && eqType typscd TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Mult)"
 where typfst = checkType env frst
       typscd = checkType env sec
checkType env (ExpDiv frst sec)
    | eqType typfst TypeNat && eqType typscd TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Div)"
 where typfst = checkType env frst
       typscd = checkType env sec
checkType env (ExpVar var) = getVarType env var	
checkType env (ExpLambda (ExpVar var) typ bdy)  
    | tkind == KindStar = TypeArrow typ bdytyp 
    | otherwise = error $ "Invalid kind (Lambda)"
 where  tkind = checkKind env typ
        nenv = addVar env var typ
     	bdytyp = checkType nenv bdy
checkType env (ExpTAbs (TypeVar tvar) kind bdy) = (TypeUniv (TypeVar tvar) kind bdytyp)
 where nenv = addTVar env tvar kind
       bdytyp = checkType nenv bdy
checkType env (ExpApp tabs (ExpTPar par))
    | parkind == ukind = substituteTypeType uvar par utyp
    | otherwise = error $ "Invalid kind (TAbs-App) "
 where TypeUniv uvar ukind utyp = checkType env tabs
       parkind                  = checkKind env par
checkType env (ExpApp lambda exp) 
    | eqType var_type exp_type = ret_type
    | otherwise = error $ "Invalid type (Abs-App)"
 where exp_type = checkType env exp
       TypeArrow var_type ret_type = checkType env lambda
checkType env (ExpDef var exp) = checkType env exp


-- Kinding
checkKind :: Env -> Type -> Kind
checkKind _ TypeBool = KindStar
checkKind _ TypeNat  = KindStar
checkKind env (TypeVar tvar) = getTVarKind env tvar
checkKind env (TypeOpAbs (TypeVar tvar) kind typ) = KindArrow kind typkind
    where typkind = checkKind nenv typ
          nenv    = addTVar env tvar kind
checkKind env (TypeOpApp t1 t2) =
    case t1kind of
        (KindArrow k1 k2) -> if k1 == t2kind
                                then k2
                                else error $ "Kind Mismatch! (OpApp)"
        _                 -> error $ "K-App Error"
    where t1kind = checkKind env t1
          t2kind = checkKind env t2
checkKind env (TypeArrow t1 t2)
    | t1kind == KindStar && t2kind == KindStar = KindStar 
    | otherwise = error $ "K-Arrow Error"
 where t1kind = checkKind env t1
       t2kind = checkKind env t2
checkKind env (TypeUniv (TypeVar tvar) kind typ)
    | typkind == KindStar = KindStar
    | otherwise = error $ "K-All Error"
 where typkind = checkKind nenv typ
       nenv    = addTVar env tvar kind



-- Type Equivalence

eqType :: Type -> Type -> Bool
eqType (TypeArrow t11 t12) (TypeArrow t21 t22) = 
    eqType t11 t21 && eqType t12 t22
eqType (TypeUniv v1 k1 t1) (TypeUniv v2 k2 t2) =
    k1 == k2 && eqType t1 t2
eqType (TypeOpAbs v1 k1 t1) (TypeOpAbs v2 k2 t2) =
    k1 == k2 && eqType t1 t2
eqType (TypeOpApp t11 t12) (TypeOpApp t21 t22) =
    eqType t11 t21 && eqType t12 t22
eqType (TypeOpApp (TypeOpAbs tvar _ t1) t2) t3 =
    eqType t3 styp
 where styp = substituteTypeType tvar t2 t1
eqType t1 (TypeOpApp (TypeOpAbs tvar kind t2) t3) =
    eqType (TypeOpApp (TypeOpAbs tvar kind t2) t3) t1
eqType t1 t2 = t1 == t2

--- Substitutions
-- Type in Type

substituteTypeType :: Type -> Type -> Type -> Type
substituteTypeType t1 t2 (TypeArrow ta1 ta2) = (TypeArrow (substituteTypeType t1 t2 ta1) (substituteTypeType t1 t2 ta2))
substituteTypeType t1 t2 (TypeUniv tvar kind typ) = (TypeUniv tvar kind (substituteTypeType t1 t2 typ))
substituteTypeType t1 t2 (TypeOpApp ta1 ta2) = (TypeOpApp (substituteTypeType t1 t2 ta1) (substituteTypeType t1 t2 ta2)) 
substituteTypeType t1 t2 (TypeOpAbs var kind typ) 
    | t1 == var = (TypeOpAbs var kind typ)
    | otherwise = (TypeOpAbs var kind (substituteTypeType t1 t2 typ)) 
substituteTypeType t1 t2 t3 
    | t1 == t3 = t2
    | otherwise = t3
-- Type in Term

