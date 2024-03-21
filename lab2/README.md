# QuickSort

Tą część wykonujemy w IntelliJ
Celem tego ćwiczenia będzie zaimplementowanie algorytmu quicksort. W module qsort utwórz następujące funkcje:

- Funkcja ```less_than/2```, która dla listy i zadanego argumentu wybierze te elementy które są mniejsze od argumentu. Wykorzystaj list comprehensions.
```
less_than(List, Arg) -> ... 
```
- Funkcja ```grt_eq_than/2```, która dla listy i zadanego argumentu wybierze te elementy które są większe bądź równe od argumentu. Tutaj też wykorzystaj list comprehensions.
```
grt_eq_than(List, Arg) -> ... 
```
- Funkcja ```qs/1``` implementująca algorytm quicksort:
```
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs(grt_eq_than(Tail,Pivot) ) 
```  
Mając zaimplementowanego quicksorta, dobrze byłoby sprawdzić jego działanie. W tym celu zaimplementuj funkcje pomocnicze ułatwiające testowanie.

- Funkcja ```random_elems/3```, która zwróci listę losowych elementów z zakresu [Min,Max] o rozmiarze N. Wykorzystaj list comprehensions oraz ```rand:uniform/1``` i ```lists:seq/2```.
```
random_elems(N,Min,Max)-> ... 
```
- Funkcja ```compare_speeds/3``` która porówna prędkości działania podanych algorytmów sortujących dla zadanej listy. Dwa ostatnie parametry to funkcje. Wykorzystaj do tego funkcję ```timer:tc```
```
compare_speeds(List, Fun1, Fun2) -> ... 
```
Interesujące nas dane wypisz na standardowe wyjście, formatując je funkcją ```io:format/2```.

# Pollution

Utwórz nowy moduł o nazwie pollution, który będzie zbierał i przetwarzał dane ze stacji mierzących jakość powietrza. Moduł powinien przechowywać:
- informacje o stacjach pomiarowych,
  - współrzędne geograficzne,
  - nazwy stacji pomiarowych,
- zmierzone wartości pomiarów, np stężenia pyłów PM10, PM2.5 czy wartości temperatury (wraz z datą i godziną pomiaru).

Nie powinno być możliwe:
- dodanie dwóch stacji pomiarowych o tej samej nazwie lub tych samych współrzędnych;
- dodanie dwóch pomiarów o tych samych:
  - współrzędnych,
  - dacie i godzinie,
  - typie (PM10, PM2.5, temperatura, …);
- dodanie pomiaru do nieistniejącej stacji.

Zaprojektuj strukturę danych dla przechowywania takich informacji (jest przynajmniej kilka dobrych rozwiązań tego problemu).

Zaimplementuj funkcje w module pollution:

- ```create_monitor/0```- tworzy i zwraca nowy monitor zanieczyszczeń;
- ```add_station/3``` - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
- ```add_value/5``` - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
- ```remove_value/4``` - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
- ```get_one_value/4``` - zwraca wartość pomiaru z zadanej stacji o zadanym typie i z zadanej daty;
- ```get_station_mean/3``` - zwraca średnią wartość parametru z zadanej stacji i danego typu;
- ```get_daily_mean/3``` - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;
