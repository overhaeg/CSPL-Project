all: clean-all Parser.hs Evaluator.hs TypeChecker.hs Testing.hs Main.hs
	   ghc -fhpc Testing.hs --make
	   ghc Main.hs
	   ./Testing
	   hpc markup Testing --exclude=Main

Parser.hs: Parser.y
	   happy -i Parser.y

clean: 
	   -rm Parser.hs
	   -rm Parser.info
	   -rm *.o
	   -rm *.hi
	   -rm *.html
	   -rm *.tix

clean-all: clean
	   -rm Main
	   -rm Testing	

