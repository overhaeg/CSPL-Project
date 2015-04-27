all: Parser.hs Evaluator.hs TypeChecker.hs Testing.hs Main.hs
	   ghc -fhpc Testing.hs --make


Parser.hs: Parser.y
	   happy -i Parser.y

clean: 
	   -rm Parser.hs
	   -rm Parser.info
	   -rm *.o
	   -rm *.hi

clean-all: clean
	   -rm Main
	   -rm Testing	

