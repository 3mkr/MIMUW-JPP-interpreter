.PHONY: clean

interpreter:
	ghc -o interpreter --make -isrc/ -isrc/Utils -isrc/GrammarFiles Main.hs
	rm -f *.o *.hi
	rm -f ./src/*.o
	rm -f ./src/*.hi
	rm -f ./src/Utils/*.o
	rm -f ./src/Utils/*.hi
	rm -f ./src/GrammarFiles/*.o
	rm -f ./src/GrammarFiles/*.hi

fast: 
	ghc -o interpreter --make -isrc/ -isrc/Utils -isrc/GrammarFiles Main.hs

clean:
	rm -f Interpreter *.o *.hi
	rm -f ./src/*.o
	rm -f ./src/*.hi
	rm -f ./src/Utils/*.o
	rm -f ./src/Utils/*.hi
	rm -f ./src/GrammarFiles/*.o
	rm -f ./src/GrammarFiles/*.hi
