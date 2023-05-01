.PHONY: clean

interpreter:
	ghc -o interpreter --make -iscr/ -isrc/Utils -isrc/GrammarFiles Main.hs
	rm -f *.o *.hi
	rm -f ./src/Utils/*.o
	rm -f ./src/Utils/*.hi
	rm -f ./src/GrammarFiles/*.o
	rm -f ./src/GrammarFiles/*.hi

clean:
	rm -f Interpreter *.o *.hi
