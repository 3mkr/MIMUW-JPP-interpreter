To create grammar files:
cd ./src/GrammarFiles
bnfc -m --functor Hint.cf
alex --ghc LexHint.x
happy --ghc --coerce --array --info ParHint.y