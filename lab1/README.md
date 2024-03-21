# Moduły i funkcje
- Uruchom IntelliJ, załóż nowy projekt Erlang,
- Utwórz nowy moduł, czyli nowy plik z rozszerzeniem .erl w katalogu src
- W module zaimplementuj funkcję power/2, która podniesie pierwszy argument do potęgi podanej w drugim parametrze.
- Przetestuj jej działanie w konsoli; w tym celu utwórz nową konfiguracje uruchomieniową typu Erlang Console.
- Utwórz nowy moduł o nazwie myLists. Zaimplementuj i przetestuj funkcje:
  - ```contains/2```, która jako parametry weźmie listę i wartość, i zwróci true jeśli lista zawiera wartość.
  - ```duplicateElements/1```, która zwróci listę zawierającą każdy z elementów dwukrotnie - [A, B, …] zmienia w [A, A, B, B, …].
  - ```sumFloats/1```, która zsumuje elementy będące liczbami zmiennoprzecinkowymi.
- Zmodyfikuj funkcję ```sumFloats/1``` by korzystała z rekurencji ogonowej.

# Kalkulator zanieczyszczenia

- Na potrzeby testowania zdefiniuj funkcje zwracającą własne, przykładowe dane, zgodne ze strukturą z zadania 3. Dane powinny dotyczyć kilku dni, kilku stacji i kilku rodzajów pomiarów.
- Zdefiniuj moduł pozwalający na przetwarzanie danych o jakości powietrza na podstawie listy pomiarów. Moduł dostarczać ma funkcje:
  - ```number_of_readings(Readings, Date) -> int```
  - ```calculate_max(Readings, Type) -> float```
  - ```calculate_mean(Readings, Type) -> float```
- Funkcje mają być zabezpieczone przed podaniem nieistniejącego typu pomiaru.