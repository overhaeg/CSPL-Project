module TypeChecker where
import qualified Parser as P


data Type = Bool
          | Nat
    deriving (Show, Eq)

checkType :: P.Exp -> Type
checkType (P.ValBool _) = Bool
checkType (P.ValNat _) = Nat
checkType (P.ExpIsZero exp)
    | checkType exp == Nat = Bool
    | otherwise = error $ "Invalid type (iszero)"
checkType (P.ExpSucc exp)
    | checkType exp == Nat = Nat
    | otherwise = error $ "Invalid type (succ)"
checkType (P.ExpPred exp)
    | checkType exp == Nat = Nat
    | otherwise = error $ "Invalid type (pred)"
checkType (P.ExpIf cond frst sec) 
    | checkType cond == Bool && checkType frst == checkType sec = checkType frst
    | otherwise = error $ "Invalid type (If)"
checkType (P.ExpPlus frst sec)
    | checkType frst == Nat && checkType sec == Nat = Nat
    | otherwise = error $ "Invalid type (Plus)"
checkType (P.ExpMin frst sec)
    | checkType frst == Nat && checkType sec == Nat = Nat
    | otherwise = error $ "Invalid type (Min)" 
checkType (P.ExpMult frst sec)
    | checkType frst == Nat && checkType sec == Nat = Nat
    | otherwise = error $ "Invalid type (Mult)"
checkType (P.ExpDiv frst sec)
    | checkType frst == Nat && checkType sec == Nat = Nat
    | otherwise = error $ "Invalid type (Div)"

