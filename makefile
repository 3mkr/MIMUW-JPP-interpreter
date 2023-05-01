.PHONY: clean

interpreter:
	ghc -o interpreter --make -iscr/ -isrc/Utils -isrc/GrammarFiles Main.hs

clean:
	rm -f Interpreter *.o *.hi
