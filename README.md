Własności języka Hint
---------------------
Opisywany język jest oparty na C / Latte.

Interpreter jest uruchamiany poleceniem

    ./interpreter PROGRAM

PROGRAM to nazwa pliku, zawierającego program do interpretacji. Proghram posiada instrukcje obsługujące standardowe wejście i nie interpretuje programów w nim pisanych pisanych.

Aby wykonać testy, należy uruchomić skrypt test.sh. KOnieczne może być nadanie uprawnień do wykonywania:
    chmod 700 test.sh

Funkcja main() jest typu void.

Bloki pętli, instrukcji warunkowej if oraz funkcji muszą być otoczone klamrami { } ze względu na czytelność.

Puste tablice muszą zawierać spację pomiędzy nawiasami kwadratowymi.
    int[] arr = [ ]     //Poprawna pusta tablica
    int[] arr = []      //Niepoprawna pusta tablica

Typy krotek są oznaczane za pomocą nawiasów ostrokątnych <>, a wartości za pomocą znaków zapytania ? ?, dla wizualnego odróżnienia od innych konstrukcji. Ponieważ nie zaimplementowałem pattern-matchingu na krotkach są one jedynie ciekawostką niezgłoszoną do oceny i nie są objęte typowaniem.

    tuple<int, string> example = ? 7, "some_string" ?

Tablice są deklarowane bez podawania rozmiaru.

Zmienne read-only występują na ten moment tylko w pętli for i są domyślnie typu Int.

    for (i := s to e) { ... }

Program akceptuje parametry wywołania typów int, boolean oraz string (na potrzeby parsowania napisy muszą być otoczone '*').

Program posiada obsługę standardowego wejścia

    scan(x);

Polecenie to wczytuje wartość z wejścia i zapisuje ją w zmiennej x (tu również napisy należy otoczyć '*').

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

Uwagi do oceny
--------------

Testy
 * 07-argumentyWywołania-01
 * 07-inOut-02

są zależne od inputu użytkownika, więc nie są częścią automatycznego testowania.

Tabela cech
-----------
  Na 15 punktów
  + 01 (trzy typy)
  + 02 (literały, arytmetyka, porównania)
  + 03 (zmienne, przypisanie)
  + 04 (print)
  + 05 (while, if)
  + 06 (funkcje lub procedury, rekurencja)
  ? 07 (przez zmienną / przez wartość / in/out)
  + 08 (zmienne read-only i pętla for)
  Na 20 punktów
  + 09 (przesłanianie i statyczne wiązanie)
  + 10 (obsługa błędów wykonania)
  + 11 (funkcje zwracające wartość)
  Na 30 punktów
  + 12 (4) (statyczne typowanie)
    13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  + 14 (1) (rekordy/listy/tablice/tablice wielowymiarowe)
    15 (2) (krotki z przypisaniem)
  + 16 (1) (break, continue)
    17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
    18 (3) (generatory)
  Dodatkowe
  + 19 (2) (printf)

Razem: 28 / 30

