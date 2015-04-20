module Evaluator where
import qualified Parser as P



-- Base Rules
eval(P.ValBool P.BoolFalse) = P.ValBool P.BoolFalse
eval(P.ValBool P.BoolTrue)  = P.ValBool P.BoolTrue
eval(P.ValNat  P.NatZero)   = P.ValNat P.NatZero

-- E-Succ
eval(P.ExpSucc e) = P.ExpSucc (eval e)

-- E-Pred + E-PredZero + E-PredSucc 
eval(P.ExpPred e) = exp_pred (eval e)
	where exp_pred (P.ValNat P.NatZero) = P.ValNat P.NatZero
      	      exp_pred (P.ExpSucc val)      = val


-- E-IsZero + E-IsZeroSucc + E-IsZeroZero
eval(P.ExpIsZero e) = exp_iszero (eval e)
	where exp_iszero (P.ValNat P.NatZero) = P.ValBool P.BoolTrue
      	      exp_iszero (P.ExpSucc _)        = P.ValBool P.BoolFalse
 

-- E-If + E-IfTrue + E-IfFalse
eval(P.ExpIf c f s) = exp_if (eval c)
	where exp_if (P.ValBool P.BoolTrue)  = eval f
      	      exp_if (P.ValBool P.BoolFalse) = eval s

-- Addition
eval(P.ExpPlus frst (P.ValNat P.NatZero)) = eval frst
eval(P.ExpPlus frst (P.ExpSucc sec))      = eval (P.ExpSucc (P.ExpPlus frst sec))
eval(P.ExpPlus frst sec)                  = eval (P.ExpPlus (eval frst) (eval sec))

-- Substraction
eval(P.ExpMin frst (P.ValNat P.NatZero))        = eval frst
eval(P.ExpMin (P.ValNat P.NatZero) _)           = P.ValNat P.NatZero
eval(P.ExpMin (P.ExpSucc frst) (P.ExpSucc sec)) = eval (P.ExpMin frst sec)
eval(P.ExpMin frst sec)                         = eval (P.ExpMin (eval frst) (eval sec))

  
-- Multiplication
eval(P.ExpMult frst (P.ValNat P.NatZero)) = P.ValNat P.NatZero
eval(P.ExpMult frst (P.ExpSucc sec))      = eval (P.ExpPlus frst (P.ExpMult frst sec))
eval(P.ExpMult frst sec)                  = eval (P.ExpMult (eval frst) (eval sec))

-- Division
eval(P.ExpDiv frst (P.ValNat P.NatZero))        = error $ "Divide by zero"
eval(P.ExpDiv (P.ValNat P.NatZero) sec)         = P.ValNat P.NatZero
eval(P.ExpDiv (P.ExpSucc frst) (P.ExpSucc sec)) = eval (P.ExpSucc (P.ExpDiv (P.ExpMin frst sec) (P.ExpSucc sec)))
eval(P.ExpDiv frst sec)                         = eval (P.ExpDiv (eval frst) (eval sec)) 
