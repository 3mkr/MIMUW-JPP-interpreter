Własności języka Hint
---------------------
Opisywany język jest oparty na C / Latte.

Interpreter jest uruchamiany poleceniem

    ./interpreter PROGRAM

, gdize PROGRAM to nazwa pliku, zawierającego program do interpretacji. Proghram posiada instrukcje obsługujące standardowe wejście i nie interpretuje programów w nim pisanych pisanych.

Program posiada statyczny TypeChecker, który jest obecnie zrobiony w około 80%.

Bloki pętli, instrukcji warunkowej if oraz funkcji muszą być otoczone klamrami { } ze względu na czytelność.

Typy krotek są oznaczane za pomocą nawiasów ostrokątnych <>, a wartości za pomocą znaków zapytania ? ?, dla wizualnego odróżnienia od innych konstrukcji. Ponieważ nie zaimplementowałem pattern-matchingu na krotkach są one jedynie ciekawostką spoza zakresu i nie są objęte typowaniem.

    tuple<int, string> example = ? 7, "some_string" ?

Tablice są deklarowane bez podawania rozmiaru.

Zmienne read-only występują na ten moment tylko w pętli for i są domyślnie typu Int.

    for (i := s to e) { ... }

Program akceptuje parametry wywołania typów int, boolean oraz string (na potrzeby parsowania napisy muszą być otoczone '*')

Wielokrotne deklaracje tej samej zmiennej są akceptowane, ale deklaracja bez inicjalizacji już zadeklarowanej zmiennej powoduje błąd.

Poprawny kod:

    int x;
    int x = 42;
    int x = 10;
    string x = "ala ma kota";

Niepoprawny kod:

    int x = 42;
    int x;

Aby utworzyć pliki gramatyki:

    cd ./src/GrammarFiles
    bnfc -m --functor Hint.cf
    alex --ghc LexHint.x
    happy --ghc --coerce --array --info ParHint.y

