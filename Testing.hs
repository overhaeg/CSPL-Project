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

-- Test List

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

-- Check Valid Typing

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

-- Check Invalid typing

cf_test1 = assertError "IsZero Bool Fail" "Invalid type (iszero)" (check "iszero true")
cf_test2 = assertError "If not the same type" "Invalid type (If)" (check "if true then true else 0")
cf_test3 = assertError "If cond not Bool" "Invalid type (If)" (check "if 0 then true else false")


-- Test Lists

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
                      TestLabel "Tfcheck3" cf_test3]


------ Eval tests

ev text = eval . parse $ text

e_test1 = TestCase (assertEqual "Echeck 0" (ev "0") (ValNat NatZero))
e_test2 = TestCase (assertEqual "Echeck true" (ev "true") (ValBool BoolTrue))
e_test3 = TestCase (assertEqual "Echeck iszero 0" (ev "iszero 0") (ValBool BoolTrue))
e_test4 = TestCase (assertEqual "Echeck iszero succ 0" (ev "iszero succ 0") (ValBool BoolFalse))
e_test5 = TestCase (assertEqual "Echeck iszero pred 0" (ev "iszero pred 0") (ValBool BoolTrue))
e_test6 = TestCase (assertEqual "Echeck ifTrue" (ev "if iszero 0 then succ 0 else succ succ 0") (ExpSucc (ValNat NatZero)))
e_test7 = TestCase (assertEqual "Echeck IfFalse" (ev "if iszero succ 0 then 0 else pred 0") (ValNat NatZero))
e_test8 = TestCase (assertEqual "Echeck Plus Zero" (ev "plus 0 0") (ValNat NatZero))
e_test9 = TestCase (assertEqual "Echeck Plus Succ" (ev "plus succ 0 0") (ExpSucc (ValNat NatZero)))
e_test10 = TestCase (assertEqual "Echeck Plus Gen" (ev "plus succ 0 succ 0") (ExpSucc (ExpSucc (ValNat NatZero))))
e_test11 = TestCase (assertEqual "Echeck Min Zero" (ev "min 0 0") (ValNat NatZero))
e_test12 = TestCase (assertEqual "Echeck Min Succ" (ev "min succ 0 0") (ExpSucc (ValNat NatZero)))
e_test13 = TestCase (assertEqual "Echeck Min Gen" (ev "min succ succ 0 succ 0") (ExpSucc (ValNat NatZero)))
e_test14 = TestCase (assertEqual "Echeck Mult Zero" (ev "mult 0 0") (ValNat NatZero))
e_test15 = TestCase (assertEqual "Echeck Mult Succ" (ev "mult succ 0 0") (ValNat NatZero))
e_test16 = TestCase (assertEqual "Echeck Mult Gen" (ev "mult succ succ 0 succ succ 0") (ExpSucc (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero))))))
e_test17 = TestCase (assertEqual "Echeck Div Zero" (ev "div 0 succ 0") (ValNat NatZero))
e_test18 = TestCase (assertEqual "Echeck Div Succ" (ev "div succ 0 succ 0") (ExpSucc (ValNat NatZero)))
e_test19 = TestCase (assertEqual "Echeck Div Gen" (ev "div succ succ succ succ 0 succ succ 0") (ExpSucc (ExpSucc (ValNat NatZero)))) 

e_tests = TestList [TestLabel "Echeck 1" e_test1,
		    TestLabel "Echeck 2" e_test2,
		    TestLabel "Echeck 3" e_test3,
		    TestLabel "Echeck 4" e_test4,
		    TestLabel "Echeck 5" e_test5,
		    TestLabel "Echeck 6" e_test6,
		    TestLabel "Echeck 7" e_test7,
		    TestLabel "Echeck 8" e_test8,
		    TestLabel "Echeck 9" e_test9,		    
		    TestLabel "Echeck 10" e_test10,
		    TestLabel "Echeck 11" e_test11,
		    TestLabel "Echeck 12" e_test12,
		    TestLabel "Echeck 13" e_test13,
		    TestLabel "Echeck 14" e_test14,
		    TestLabel "Echeck 15" e_test15,
		    TestLabel "Echeck 16" e_test16,		    
		    TestLabel "Echeck 17" e_test17,
		    TestLabel "Echeck 18" e_test18,
		    TestLabel "Echeck 19" e_test19]
		 

		
main = do
	putStrLn "Testing Parser: " 
	runTestTT p_tests
	putStrLn "\n \nTesting TypeChecker: " 
        runTestTT c_tests
	putStrLn "\n \nTesting TypeChecker (Failures): "
        runTestTT cf_tests
	putStrLn "\n \nTest Evaluator: "
        runTestTT e_tests
