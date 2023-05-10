Własności języka Hint
---------------------
Opisywany język jest oparty na C / Latte.

Bloki pętli, instrukcji warunkowej if oraz funkcji muszą być otoczone klamrami { } ze względu
na czytelność.

Typy krotek są oznaczane za pomocą nawiasów ostrokątnych <>, a wartości za pomocą znaków zapytania ? ?, dla wizualnego odróżnienia od innych konstrukcji.

    tuple<int, string> example = ? 7, "some_string" ?

Tablice są deklarowane bez podawania rozmiaru.

Zmienne read-only występują na ten moment tylko w pętli for i są domyślnie typu Int.

    for (i := s to e) { ... }


Aby utworzyć pliki gramatyki:

    cd ./src/GrammarFiles
    bnfc -m --functor Hint.cf
    alex --ghc LexHint.x
    happy --ghc --coerce --array --info ParHint.y

