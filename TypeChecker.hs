module TypeChecker where
import           Parser
import qualified Data.Map.Strict as Map

type Env = Map.Map String Type

addVar :: Env -> String -> Type -> Env 
addVar env var typ = 
	Map.insert var typ env 

checkType :: Env -> Exp -> Type
checkType _ (ValBool _) = TypeBool
checkType _ (ValNat _) = TypeNat
checkType env (ExpIsZero exp)
    | checkType env exp == TypeNat = TypeBool
    | otherwise = error $ "Invalid type (iszero)"
checkType env (ExpSucc exp)
    | checkType env exp == TypeNat = TypeNat
    | otherwise = error $ "Invalid type (succ)"
checkType env (ExpPred exp)
    | checkType env exp == TypeNat = TypeNat
    | otherwise = error $ "Invalid type (pred)"
checkType env (ExpIf cond frst sec) 
    | checkType env cond == TypeBool && checkType env frst == checkType env sec = checkType env frst
    | otherwise = error $ "Invalid type (If)"
checkType env (ExpPlus frst sec)
    | checkType env frst == TypeNat && checkType env sec == TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Plus)"
checkType env (ExpMin frst sec)
    | checkType env frst == TypeNat && checkType env sec == TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Min)" 
checkType env (ExpMult frst sec)
    | checkType env frst == TypeNat && checkType env sec == TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Mult)"
checkType env (ExpDiv frst sec)
    | checkType env frst == TypeNat && checkType env sec == TypeNat = TypeNat
    | otherwise = error $ "Invalid type (Div)"
checkType env (ExpLambda (ExpVar var) typ bdy) = TypeArrow typ bdytyp 
 where  newenv = addVar env var typ
	bdytyp = checkType newenv bdy
checkType env (ExpVar var) 
    | Map.member var env = env Map.! var
    | otherwise = error $ "Unknown Var " ++ var	
checkType env (ExpApp lambda exp) 
    | var_type == exp_type = ret_type
    | otherwise = error $ "Type Mismatch! (App)"
 where exp_type = checkType env exp
       TypeArrow var_type ret_type = checkType env lambda

   

