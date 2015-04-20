module Main where

import Parser      
import TypeChecker 
import Evaluator   
import Test.HUnit
import Test.HUnit.Tools 
import Control.Exception

instance Eq ErrorCall where
    x == y = (show x) == (show y)

assertError msg ex f = 
    TestCase $ assertRaises msg (ErrorCall ex) $ evaluate f


---- Parser tests

parse text = calc . lexer $ text


p_test1 = TestCase (assertEqual "Parse 0" (parse "0") (ValNat NatZero))
p_test2 = TestCase (assertEqual "Parse true" (parse "true") (ValBool BoolTrue))
p_test3 = TestCase (assertEqual "Parse false" (parse "false") (ValBool BoolFalse))
p_test4 = TestCase (assertEqual "Parse iszero 0" (parse "iszero 0") (ExpIsZero (ValNat NatZero)))
p_test5 = TestCase (assertEqual "Parse succ 0" (parse "succ 0") (ExpSucc (ValNat NatZero)))
p_test6 = TestCase (assertEqual "Parse pred 0" (parse "pred 0") (ExpPred (ValNat NatZero)))
p_test7 = TestCase (assertEqual "Parse If" (parse "if true then succ 0 else pred 0") (ExpIf (ValBool BoolTrue) (ExpSucc (ValNat NatZero)) (ExpPred (ValNat NatZero))))
p_test8 = TestCase (assertEqual "Parse Plus" (parse "plus succ 0 0") (ExpPlus (ExpSucc (ValNat NatZero)) (ValNat NatZero)))
p_test9 = TestCase (assertEqual "Parse Min" (parse "min succ succ succ 0 succ succ 0") (ExpMin (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero)))) (ExpSucc (ExpSucc (ValNat NatZero)))))
p_test10 = TestCase (assertEqual "Parse Mult" (parse "mult succ succ succ 0 succ succ 0") (ExpMult (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero)))) (ExpSucc (ExpSucc (ValNat NatZero)))))
p_test11 = TestCase (assertEqual "Parse Div" (parse "div succ succ 0 0")  (ExpDiv (ExpSucc (ExpSucc (ValNat NatZero))) (ValNat NatZero)))

-- List

p_tests = TestList [TestLabel "Parse 1" p_test1,
	            TestLabel "Parse 2" p_test2,
		    TestLabel "Parse 3" p_test3,
	            TestLabel "Parse 4" p_test4,
		    TestLabel "Parse 5" p_test5,
	            TestLabel "Parse 6" p_test6,
	            TestLabel "Parse 7" p_test7,
		    TestLabel "Parse 8" p_test8,
		    TestLabel "Parse 9" p_test9,
		    TestLabel "Parse 10" p_test10,
                    TestLabel "Parse 11" p_test11]


------ TypeCheck tests

check text = checkType . parse $ text

-- Test Valid Typing

c_test1 = TestCase (assertEqual "Tcheck 0" (check "0") (Nat))
c_test2 = TestCase (assertEqual "Tcheck true" (check "true") (Bool))
c_test3 = TestCase (assertEqual "Tcheck false" (check "false") (Bool))
c_test4 = TestCase (assertEqual "Tcheck iszero" (check "iszero succ succ 0") (Bool))
c_test5 = TestCase (assertEqual "Tcheck if" (check "if iszero 0 then succ 0 else pred succ 0") (Nat))
c_test6 = TestCase (assertEqual "Tcheck Plus zero" (check "plus 0 0") (Nat))
c_test7 = TestCase (assertEqual "Tcheck Plus gen" (check "plus succ succ 0 0") (Nat))
c_test8 = TestCase (assertEqual "Tcheck Min zero"(check "min 0 0") (Nat))
c_test9 = TestCase (assertEqual "Tcheck Min gen" (check "min succ succ succ 0 succ succ 0") (Nat))
c_test10 = TestCase (assertEqual "Tcheck Mult zero" (check "mult 0 0") (Nat))
c_test11 = TestCase (assertEqual "Tcheck Mult gen" (check "mult succ 0 0 ") (Nat))
c_test12 = TestCase (assertEqual "Tcheck Div zero" (check "div 0 succ 0") (Nat))
c_test13 = TestCase (assertEqual "Tcheck Div gen" (check "div succ succ 0 succ 0") (Nat))

-- Test Invalid typing

cf_test1 = assertError "IsZero Bool Fail" "Invalid type (iszero)" (check "iszero true")
cf_test2 = assertError "If not the same type" "Invalid type (If)" (check "if true then true else 0")
cf_test3 = assertError "If cond not Bool" "Invalid type (If)" (check "if 0 then true else false")
cf_test4 = assertError "plus bool" "Invalid type (Plus)" (check "plus true 0")
cf_test5 = assertError "min bool" "Invalid type (Min)" (check "min true 0")
cf_test6 = assertError "mult bool" "Invalid type (Mult)" (check "mult true 0")
cf_test7 = assertError "div bool" "Invalid type (Div)" (check "div 0 true")

-- Lists

c_tests = TestList [TestLabel "Tcheck 1" c_test1,
		    TestLabel "Tcheck 2" c_test2,
	            TestLabel "Tcheck 3" c_test3,
		    TestLabel "Tcheck 4" c_test4,
		    TestLabel "Tcheck 5" c_test5,
		    TestLabel "Tcheck 6" c_test6,
		    TestLabel "Tcheck 7" c_test7,
	            TestLabel "Tcheck 8" c_test8,
		    TestLabel "Tcheck 9" c_test9,
		    TestLabel "Tcheck 10" c_test10,
		    TestLabel "Tcheck 11" c_test11,
		    TestLabel "Tcheck 12" c_test12,
                    TestLabel "Tcheck 13" c_test13]	

cf_tests = TestList  [TestLabel "TFcheck1" cf_test1,
                      TestLabel "Tfcheck2" cf_test2,
                      TestLabel "Tfcheck3" cf_test3,
  		      TestLabel "Tfcheck4" cf_test4,
                      TestLabel "Tfcheck5" cf_test5,
                      TestLabel "Tfcheck6" cf_test6,
                      TestLabel "Tfcheck7" cf_test7]


------ Eval tests

ev text = eval . parse $ text

-- Test valid evaluations

e_test1 = TestCase (assertEqual "Eval 0" (ev "0") (ValNat NatZero))
e_test2 = TestCase (assertEqual "Eval true" (ev "true") (ValBool BoolTrue))
e_test3 = TestCase (assertEqual "Eval iszero 0" (ev "iszero 0") (ValBool BoolTrue))
e_test4 = TestCase (assertEqual "Eval iszero succ 0" (ev "iszero succ 0") (ValBool BoolFalse))
e_test5 = TestCase (assertEqual "Eval iszero pred 0" (ev "iszero pred 0") (ValBool BoolTrue))
e_test6 = TestCase (assertEqual "Eval ifTrue" (ev "if iszero 0 then succ 0 else succ succ 0") (ExpSucc (ValNat NatZero)))
e_test7 = TestCase (assertEqual "Eval IfFalse" (ev "if iszero succ 0 then 0 else pred 0") (ValNat NatZero))
e_test8 = TestCase (assertEqual "Eval Plus Zero" (ev "plus 0 0") (ValNat NatZero))
e_test9 = TestCase (assertEqual "Eval Plus Succ" (ev "plus succ 0 0") (ExpSucc (ValNat NatZero)))
e_test10 = TestCase (assertEqual "Eval Plus Gen" (ev "plus succ 0 succ 0") (ExpSucc (ExpSucc (ValNat NatZero))))
e_test11 = TestCase (assertEqual "Eval Min Zero" (ev "min 0 0") (ValNat NatZero))
e_test12 = TestCase (assertEqual "Eval Min Succ" (ev "min succ 0 0") (ExpSucc (ValNat NatZero)))
e_test13 = TestCase (assertEqual "Eval Min Gen" (ev "min succ succ 0 succ 0") (ExpSucc (ValNat NatZero)))
e_test14 = TestCase (assertEqual "Eval Mult Zero" (ev "mult 0 0") (ValNat NatZero))
e_test15 = TestCase (assertEqual "Eval Mult Succ" (ev "mult succ 0 0") (ValNat NatZero))
e_test16 = TestCase (assertEqual "Eval Mult Gen" (ev "mult succ succ 0 succ succ 0") (ExpSucc (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero))))))
e_test17 = TestCase (assertEqual "Eval Div Zero" (ev "div 0 succ 0") (ValNat NatZero))
e_test18 = TestCase (assertEqual "Eval Div Succ" (ev "div succ 0 succ 0") (ExpSucc (ValNat NatZero)))
e_test19 = TestCase (assertEqual "Eval Div Gen" (ev "div succ succ succ succ 0 succ succ 0") (ExpSucc (ExpSucc (ValNat NatZero)))) 


ef_test1 = assertError "Eval Div zero zero" "Divide by zero" (ev "div 0 0")
ef_test2 = assertError "Eval Div Succ zero" "Divide by zero" (ev "div succ 0 0")

e_tests = TestList [TestLabel "Eval 1" e_test1,
		    TestLabel "Eval 2" e_test2,
		    TestLabel "Eval 3" e_test3,
		    TestLabel "Eval 4" e_test4,
		    TestLabel "Eval 5" e_test5,
		    TestLabel "Eval 6" e_test6,
		    TestLabel "Eval 7" e_test7,
		    TestLabel "Eval 8" e_test8,
		    TestLabel "Eval 9" e_test9,		    
		    TestLabel "Eval 10" e_test10,
		    TestLabel "Eval 11" e_test11,
		    TestLabel "Eval 12" e_test12,
		    TestLabel "Eval 13" e_test13,
		    TestLabel "Eval 14" e_test14,
		    TestLabel "Eval 15" e_test15,
		    TestLabel "Eval 16" e_test16,		    
		    TestLabel "Eval 17" e_test17,
		    TestLabel "Eval 18" e_test18,
		    TestLabel "Eval 19" e_test19]

ef_tests = TestList [TestLabel "Eval Failure 1" ef_test1,
		     TestLabel "Eval Failure 2" ef_test2]
		 

		
main = do
	putStrLn "Testing Parser: " 
	runTestTT p_tests
	putStrLn "\n \nTesting TypeChecker: " 
        runTestTT c_tests
	putStrLn "\n \nTesting TypeChecker (Failures): "
        runTestTT cf_tests
	putStrLn "\n \nTest Evaluator: "
        runTestTT e_tests
	putStrLn "\n \nTest Evaluator (Failures): "
	runTestTT ef_tests
