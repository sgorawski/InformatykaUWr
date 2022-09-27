JP projekt
==========

Autor: Sławomir Górawski

Instrukcja
----------

Budowanie:

    make

Czyszczenie folderu:

    make clean

Uruchomienie w trybie interpretera:

    ./f test.f

Uruchomienie w trybie porównywarki:

    ./f --cmp cmptest.f

Zapisanie wyniku do pliku:

    ./f test.f > out.txt

Struktura
---------

- Syntax: definicja termów
- Lexer: lexer w formacie `ocamllex`
- Parser: parser w formacie `ocamlyacc`
- Printer: pretty-printer
- Eval: logika: odcukrzanie, ewaluator i porównywarka
- Main: uruchomienie programu, operacje na plikach

Zakres
------

Zadanie zostało zrealizowane w pełni.

Plik `test.f` zawiera testy do interpretera, a `cmptest.f` testy do porównywarki.

Język źródłowy wykorzystuje autorską składnię. Uproszczona definicja:

    e ::= \x. e  Abstrakcja
        | fix e  Operator unarny
        | a + a  Operator binarny
        | a a    Aplikacja
        | a      Atom

    a ::= x      Zmienna
        | 42     Liczba
        | true | false | []
        | ( e )

Aplikacja oraz binarne operatory wymagają nawiasów: `x y z` trzeba zapisać jako `(x y) z`.
(Wyjątkiem jest konstruktor listy).

Zakładałem, że nazwy zmiennych wolnych mają znaczenie,
tj. termy otwarte `\x. a` i `\x. b` nie są równe,
ale `\x. a` i `(\f. \s. a) b` są.

Termy w plikach źródłowych muszą być zakończone średnikiem.
Linijki zaczynające się od `#` to komentarze.
