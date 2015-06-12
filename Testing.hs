module Main where
import           Parser      
import           TypeChecker 
import           Evaluator   
import           Test.HUnit
import           Test.HUnit.Tools 
import           Control.Exception
import qualified Data.Map.Strict as Map

instance Eq ErrorCall where
    x == y = (show x) == (show y)

assertError msg ex f = 
    TestCase $ assertRaises msg (ErrorCall ex) $ evaluate f


---- Parser tests

parse text = calc . lexer $ text


p_test1 = TestCase (assertEqual "Parse 0"            (parse "0") 
						     (ValNat NatZero))
p_test2 = TestCase (assertEqual "Parse true"         (parse "true") 
						     (ValBool BoolTrue))
p_test3 = TestCase (assertEqual "Parse false"        (parse "false") 
						     (ValBool BoolFalse))
p_test4 = TestCase (assertEqual "Parse iszero 0"     (parse "iszero 0") 
						     (ExpIsZero (ValNat NatZero)))
p_test5 = TestCase (assertEqual "Parse succ 0"       (parse "succ 0") 
						     (ExpSucc (ValNat NatZero)))
p_test6 = TestCase (assertEqual "Parse pred 0"       (parse "pred 0") 
						     (ExpPred (ValNat NatZero)))
p_test7 = TestCase (assertEqual "Parse If"           (parse "if true then succ 0 else pred 0") 
						     (ExpIf (ValBool BoolTrue) 
							    (ExpSucc (ValNat NatZero)) 
				                            (ExpPred (ValNat NatZero))))
p_test8 = TestCase (assertEqual "Parse Plus"         (parse "plus succ 0 0") 
						     (ExpPlus (ExpSucc (ValNat NatZero)) 
							      (ValNat NatZero)))
p_test9 = TestCase (assertEqual "Parse Min"          (parse "min succ succ succ 0 succ succ 0") 
						     (ExpMin (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero)))) 
							     (ExpSucc (ExpSucc (ValNat NatZero)))))
p_test10 = TestCase (assertEqual "Parse Mult"        (parse "mult succ succ succ 0 succ succ 0") 
						     (ExpMult (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero)))) 
							      (ExpSucc (ExpSucc (ValNat NatZero)))))
p_test11 = TestCase (assertEqual "Parse Div"         (parse "div succ succ 0 0")  
						     (ExpDiv (ExpSucc (ExpSucc (ValNat NatZero))) 
						             (ValNat NatZero)))
p_test12 = TestCase (assertEqual "Parse Lambda"      (parse "(lambda x:Nat . 0)") 
						     (ExpLambda "x" TypeNat (ValNat NatZero)))
p_test13 = TestCase (assertEqual "Parse App"         (parse "((lambda x:Nat . succ x) 0)") 
					             (ExpApp (ExpLambda "x" TypeNat (ExpSucc (ExpVar "x"))) (ValNat NatZero))) 
p_test14 = TestCase (assertEqual "Parse Succ Lambda" (parse "(((lambda x:Nat . (lambda y:Nat . plus x y)) succ succ 0) succ succ succ 0)")
						     (ExpApp (ExpApp (ExpLambda "x" TypeNat (ExpLambda "y" TypeNat (ExpPlus (ExpVar "x") (ExpVar "y")))) 
		                                                     (ExpSucc (ExpSucc (ValNat NatZero)))) 
						             (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero))))))
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
                    TestLabel "Parse 11" p_test11,
		    TestLabel "Parse 12" p_test12,
		    TestLabel "Parse 13" p_test13,
		    TestLabel "Parse 14" p_test14
		   ]


------ TypeCheck tests

check text = checkType Map.empty . parse $ text

-- Test Valid Typing

c_test1 = TestCase (assertEqual  "Tcheck 0"             (check "0") 
							(TypeNat))
c_test2 = TestCase (assertEqual  "Tcheck true"          (check "true") 
							(TypeBool))
c_test3 = TestCase (assertEqual  "Tcheck false"         (check "false") 
							(TypeBool))
c_test4 = TestCase (assertEqual  "Tcheck iszero"        (check "iszero succ succ 0") 
							(TypeBool))
c_test5 = TestCase (assertEqual  "Tcheck if"            (check "if iszero 0 then succ 0 else pred succ 0") 
							(TypeNat))
c_test6 = TestCase (assertEqual  "Tcheck Plus zero"     (check "plus 0 0") 
							(TypeNat))
c_test7 = TestCase (assertEqual  "Tcheck Plus gen"      (check "plus succ succ 0 0") 
							(TypeNat))
c_test8 = TestCase (assertEqual  "Tcheck Min zero"      (check "min 0 0") 
							(TypeNat))
c_test9 = TestCase (assertEqual  "Tcheck Min gen"       (check "min succ succ succ 0 succ succ 0") 
							(TypeNat))
c_test10 = TestCase (assertEqual "Tcheck Mult zero"     (check "mult 0 0") 
							(TypeNat))
c_test11 = TestCase (assertEqual "Tcheck Mult gen"      (check "mult succ 0 0 ") 
							(TypeNat))
c_test12 = TestCase (assertEqual "Tcheck Div zero"      (check "div 0 succ 0") 
							(TypeNat))
c_test13 = TestCase (assertEqual "Tcheck Div gen"       (check "div succ succ 0 succ 0") 
							(TypeNat))
-- Following tests should return errors until STLC typechecking is implemented.
c_test14 = TestCase (assertEqual "Tcheck lambda type"   (check "(lambda x:Nat . x)") 
							(TypeArrow TypeNat TypeNat))
c_test15 = TestCase (assertEqual "Tcheck App"           (check "((lambda x:Nat . succ x) 0)") 
							(TypeNat))
c_test16 = TestCase (assertEqual "Tcheck App N->B"      (check "((lambda x:Nat . iszero x) 0)")
							(TypeBool))
c_test17 = TestCase (assertEqual "Tcheck App Double"    (check "(((lambda x:Nat . (lambda y:Nat . plus x y)) succ succ succ 0) succ succ 0)")
							(TypeNat)) 
c_test18 = TestCase (assertEqual "Tcheck Nested App"    (check "((lambda x:Nat . succ x) ((lambda y:Bool . if y then succ succ 0 else succ 0) true))")
							(TypeNat))

-- Test Invalid typing

cf_test1 =  assertError "IsZero Bool Fail"     "Invalid type (iszero)" (check "iszero true")
cf_test2 =  assertError "If not the same type" "Invalid type (If)"     (check "if true then true else 0")
cf_test3 =  assertError "If cond not Bool"     "Invalid type (If)"     (check "if 0 then true else false")
cf_test4 =  assertError "plus bool"            "Invalid type (Plus)"   (check "plus true 0")
cf_test5 =  assertError "min bool"             "Invalid type (Min)"    (check "min true 0")
cf_test6 =  assertError "mult bool"            "Invalid type (Mult)"   (check "mult true 0")
cf_test7 =  assertError "div bool"             "Invalid type (Div)"    (check "div 0 true")
cf_test8 =  assertError "lambda fail"          "Unknown Var y" 	       (check "(lambda x:Nat . y)") -- Behaves correctly when tested manually but fails for no reason here. assertError doesn't catch exceptions in subexpression 
                                                                                                    -- correctly? 
cf_test9 =  assertError "App fail"	       "Unknown Var y"         (check "((lambda x:Nat . y) 0)")
cf_test10 = assertError "Type Mismatch"    "Type Mismatch! (App)"  (check "((lambda x:Nat . x) true)")
cf_test11 = assertError "Succ Mismatch"    "Invalid type (succ)"   (check "succ true")
cf_test12 = assertError "Pred Mismatch"    "Invalid type (pred)"   (check "pred false")

-- Lists

c_tests = TestList [TestLabel "Tcheck 1"  c_test1,
		            TestLabel "Tcheck 2"  c_test2,
	                TestLabel "Tcheck 3"  c_test3,
		            TestLabel "Tcheck 4"  c_test4,
		            TestLabel "Tcheck 5"  c_test5,
		            TestLabel "Tcheck 6"  c_test6,
		            TestLabel "Tcheck 7"  c_test7,
	                TestLabel "Tcheck 8"  c_test8,
		            TestLabel "Tcheck 9"  c_test9,
		            TestLabel "Tcheck 10" c_test10,
		            TestLabel "Tcheck 11" c_test11,
		            TestLabel "Tcheck 12" c_test12,
                    TestLabel "Tcheck 13" c_test13,
		            TestLabel "Tcheck 14" c_test14,
		            TestLabel "Tcheck 15" c_test15,
		            TestLabel "Tcheck 16" c_test16,
		            TestLabel "Tcheck 17" c_test17,
		            TestLabel "Tcheck 18" c_test18
		             ]    	

cf_tests = TestList  [TestLabel "TFcheck1" cf_test1,
                      TestLabel "Tfcheck2" cf_test2,
                      TestLabel "Tfcheck3" cf_test3,
  		              TestLabel "Tfcheck4" cf_test4,
                      TestLabel "Tfcheck5" cf_test5,
                      TestLabel "Tfcheck6" cf_test6,
                      TestLabel "Tfcheck7" cf_test7,
		              TestLabel "Tfcheck8" cf_test8,
		              TestLabel "Tfcheck9"  cf_test9,
		              TestLabel "Tfcheck10" cf_test10,
                      TestLabel "Tfcheck11" cf_test11,
                      TestLabel "Tfcehck12" cf_test12
		     ]


------ Eval tests

ev text = eval . parse $ text

-- Test valid evaluations

e_test1 = TestCase (assertEqual  "Eval 0"             (ev "0") 
					              (ValNat NatZero))
e_test2 = TestCase (assertEqual  "Eval true"          (ev "true") 
					              (ValBool BoolTrue))
e_test3 = TestCase (assertEqual  "Eval iszero 0"      (ev "iszero 0") 
						      (ValBool BoolTrue))
e_test4 = TestCase (assertEqual  "Eval iszero succ 0" (ev "iszero succ 0") 
						      (ValBool BoolFalse))
e_test5 = TestCase (assertEqual  "Eval iszero pred 0" (ev "iszero pred 0") 
						      (ValBool BoolTrue))
e_test6 = TestCase (assertEqual  "Eval ifTrue"        (ev "if iszero 0 then succ 0 else succ succ 0") 
						      (ExpSucc (ValNat NatZero)))
e_test7 = TestCase (assertEqual  "Eval IfFalse"       (ev "if iszero succ 0 then 0 else pred 0") 
						      (ValNat NatZero))
e_test8 = TestCase (assertEqual  "Eval Plus Zero"     (ev "plus 0 0") 
						      (ValNat NatZero))
e_test9 = TestCase (assertEqual  "Eval Plus Succ"     (ev "plus succ 0 0") 
						      (ExpSucc (ValNat NatZero)))
e_test10 = TestCase (assertEqual "Eval Plus Gen"      (ev "plus succ 0 succ 0") 
						      (ExpSucc (ExpSucc (ValNat NatZero))))
e_test11 = TestCase (assertEqual "Eval Min Zero"      (ev "min 0 0") 
						      (ValNat NatZero))
e_test12 = TestCase (assertEqual "Eval Zero Min"      (ev "min 0 succ 0")
                              (ValNat NatZero))
e_test13 = TestCase (assertEqual "Eval Min Succ"      (ev "min succ 0 0") 
						      (ExpSucc (ValNat NatZero)))
e_test14 = TestCase (assertEqual "Eval Min Gen"       (ev "min succ succ 0 succ 0") 
						      (ExpSucc (ValNat NatZero)))
e_test15 = TestCase (assertEqual "Eval Min Pred"      (ev "min pred succ succ 0 succ 0")
                              (ValNat NatZero))
e_test16 = TestCase (assertEqual "Eval Mult Zero"     (ev "mult 0 0") 
						      (ValNat NatZero))
e_test17 = TestCase (assertEqual "Eval Mult Succ"     (ev "mult succ 0 0") 
						      (ValNat NatZero))
e_test18 = TestCase (assertEqual "Eval Mult Gen"      (ev "mult succ succ 0 succ succ 0") 
						      (ExpSucc (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero))))))
e_test19 = TestCase (assertEqual "Eval Div Zero"      (ev "div 0 succ 0") 
						      (ValNat NatZero))
e_test20 = TestCase (assertEqual "Eval Div Succ"      (ev "div succ 0 succ 0") 
                                                      (ExpSucc (ValNat NatZero)))
e_test21 = TestCase (assertEqual "Eval Div Gen"       (ev "div succ succ succ succ 0 succ succ 0")
						      (ExpSucc (ExpSucc (ValNat NatZero))))
e_test22 = TestCase (assertEqual "Eval Lambda simple" (ev "(lambda x:Nat . x)")
						      (ExpLambda "x" TypeNat (ExpVar "x")))
e_test23 = TestCase (assertEqual "Eval App simple"    (ev "((lambda x:Nat . succ x) 0)")
						      (ExpSucc (ValNat NatZero)))
e_test24 = TestCase (assertEqual "Eval App N>B"       (ev "((lambda x:Nat . iszero x) 0)")
						      (ValBool BoolTrue))
e_test25 = TestCase (assertEqual "Eval App double"    (ev "(((lambda x:Nat . (lambda y:Nat . plus x y)) succ succ succ 0) succ succ 0)")
						      (ExpSucc (ExpSucc (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero)))))))
e_test26 = TestCase (assertEqual "Eval App nested"    (ev "((lambda x:Nat . min succ succ succ 0 x) ((lambda y:Bool . if y then succ succ 0 else succ 0) true))")
						      (ExpSucc (ValNat NatZero)))
e_test27 = TestCase (assertEqual "Eval App nested L"  (ev "((lambda x:Nat . mult x ((lambda y:Bool . if y then 0 else succ succ 0) false)) succ succ 0)")
                              (ExpSucc (ExpSucc (ExpSucc (ExpSucc (ValNat NatZero))))))
e_test28 = TestCase (assertEqual "Eval App Same Var"  (ev "(((lambda x:Nat . (lambda x:Nat . succ x)) 0) 0)")
                              (ExpSucc (ValNat NatZero)))



ef_test1 = assertError "Eval Div zero zero" "Divide by zero" (ev "div 0 0")
ef_test2 = assertError "Eval Div Succ zero" "Divide by zero" (ev "div succ 0 0")

e_tests = TestList [TestLabel "Eval 1"  e_test1,
		    TestLabel "Eval 2"  e_test2,
		    TestLabel "Eval 3"  e_test3,
		    TestLabel "Eval 4"  e_test4,
		    TestLabel "Eval 5"  e_test5,
		    TestLabel "Eval 6"  e_test6,
		    TestLabel "Eval 7"  e_test7,
		    TestLabel "Eval 8"  e_test8,
		    TestLabel "Eval 9"  e_test9,		    
		    TestLabel "Eval 10" e_test10,
		    TestLabel "Eval 11" e_test11,
		    TestLabel "Eval 12" e_test12,
		    TestLabel "Eval 13" e_test13,
		    TestLabel "Eval 14" e_test14,
		    TestLabel "Eval 15" e_test15,
		    TestLabel "Eval 16" e_test16,		    
		    TestLabel "Eval 17" e_test17,
		    TestLabel "Eval 18" e_test18,
		    TestLabel "Eval 19" e_test19,
		    TestLabel "Eval 20" e_test20,
		    TestLabel "Eval 21" e_test21,
		    TestLabel "Eval 22" e_test22,
		    TestLabel "Eval 23" e_test23,
		    TestLabel "Eval 24" e_test24,
            TestLabel "Eval 25" e_test25,
            TestLabel "Eval 26" e_test26,
            TestLabel "Eval 27" e_test27,
            TestLabel "Eval 28" e_test28
    		   ]

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
