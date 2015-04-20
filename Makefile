all: Parser.hs Evaluator.hs TypeChecker.hs Testing.hs Main.hs
	   ghc Main.hs
	   ghc Testing.hs


Parser.hs: Parser.y
	   happy Parser.y

clean: 
	   -rm Parser.hs
	   -rm *.o
	   -rm *.hi

clean-all: clean
	   -rm Main
	   -rm Testing	

