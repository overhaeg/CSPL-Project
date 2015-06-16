module Evaluator where
import Parser



-- Base Rules
eval(ValBool BoolFalse) = ValBool BoolFalse
eval(ValBool BoolTrue)  = ValBool BoolTrue
eval(ValNat  NatZero)   = ValNat NatZero

-- E-Succ
eval(ExpSucc e) = ExpSucc (eval e)

-- E-Pred + E-PredZero + E-PredSucc 
eval(ExpPred e) = exp_pred (eval e)
	where exp_pred (ValNat NatZero) = ValNat NatZero
      	      exp_pred (ExpSucc val)    = val


-- E-IsZero + E-IsZeroSucc + E-IsZeroZero
eval(ExpIsZero e) = exp_iszero (eval e)
	where exp_iszero (ValNat NatZero) = ValBool BoolTrue
      	      exp_iszero (ExpSucc _)      = ValBool BoolFalse
 

-- E-If + E-IfTrue + E-IfFalse
eval(ExpIf c f s) = exp_if (eval c)
	where exp_if (ValBool BoolTrue)  = eval f
      	      exp_if (ValBool BoolFalse) = eval s

-- Addition
eval(ExpPlus frst (ValNat NatZero)) = eval frst
eval(ExpPlus frst (ExpSucc sec))    = eval (ExpSucc (ExpPlus frst sec))
eval(ExpPlus frst sec)              = eval (ExpPlus (eval frst) (eval sec))

-- Substraction
eval(ExpMin frst (ValNat NatZero))        = eval frst
eval(ExpMin (ValNat NatZero) _)           = ValNat NatZero
eval(ExpMin (ExpSucc frst) (ExpSucc sec)) = eval (ExpMin frst sec)
eval(ExpMin frst sec)                     = eval (ExpMin (eval frst) (eval sec))

  
-- Multiplication
eval(ExpMult frst (ValNat NatZero)) = ValNat NatZero
eval(ExpMult frst (ExpSucc sec))    = eval (ExpPlus frst (ExpMult frst sec))
eval(ExpMult frst sec)              = eval (ExpMult (eval frst) (eval sec))

-- Division
eval(ExpDiv frst (ValNat NatZero))        = error $ "Divide by zero"
eval(ExpDiv (ValNat NatZero) sec)         = ValNat NatZero
eval(ExpDiv (ExpSucc frst) (ExpSucc sec)) = eval (ExpSucc (ExpDiv (ExpMin frst sec) (ExpSucc sec)))
eval(ExpDiv frst sec)                     = eval (ExpDiv (eval frst) (eval sec)) 


-- Lambda
eval(ExpLambda var typ bdy)  = ExpLambda var typ bdy

-- App
-- E-AppAbs
eval(ExpApp (ExpLambda var _ bdy) term) = eval sub_exp
	where sub_exp = substitute var ev_term bdy 
	      ev_term = eval term --E-App2
-- E-App1
eval(ExpApp frst scd) = eval (ExpApp (eval frst) scd)	     

-- Substitution 

substitute var exp (ExpSucc bdy)           = ExpSucc (substitute var exp bdy)
substitute var exp (ExpPred bdy)           = ExpPred (substitute var exp bdy)
substitute var exp (ExpIsZero bdy)         = ExpIsZero (substitute var exp bdy)
substitute var exp (ExpIf c f s)           = ExpIf (substitute var exp c) (substitute var exp f) (substitute var exp s)
substitute var exp (ExpPlus frst scd)      = ExpPlus (substitute var exp frst) (substitute var exp scd)
substitute var exp (ExpMin frst scd)       = ExpMin (substitute var exp frst) (substitute var exp scd)
substitute var exp (ExpMult frst scd)      = ExpMult (substitute var exp frst) (substitute var exp scd)
substitute var exp (ExpDiv frst scd)       = ExpDiv (substitute var exp frst) (substitute var exp scd)
substitute var exp (ExpLambda v2 typ bdy)
 | var == v2  = (ExpLambda var typ bdy)
 | otherwise  = ExpLambda v2 typ (substitute var exp bdy)
substitute var exp (ExpApp lam term)       = ExpApp (substitute var exp lam) (substitute var exp term)
substitute (ExpVar var) exp (ExpVar name)
 | var == name 	= exp
 | otherwise   	= ExpVar name
substitute var exp val                     = val
