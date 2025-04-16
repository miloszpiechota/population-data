text <- "
height [cm]
weight [kg]
sex [male = 1 / female = 0]
year [1925 - 2025]
age [1 - 100]
country [ES, UAE, US, TJK, GB]
BMI = round(weight / ((height / 100) ^ 2), 2)
education = [
  based on ISCED 
  0 -> Wczesna edukacja dzieci – przedszkole lub edukacja przedformalna (wiek < 6)
  1 -> Szkoła podstawowa
  2 -> Niższe wykształcenie średnie (gimnazjum)
  3 -> Wyższe wykształcenie średnie (liceum, technikum)
  4 -> Edukacja policealna niezakończona stopniem
  5 -> Krótkie studia wyższe lub programy zawodowe
  6 -> Licencjat lub jego odpowiednik
  7 -> Magister lub jego odpowiednik
  8 -> Doktorat
  None -> Brak danych o edukacji / brak formalnego wykształcenia
]

Nasz zbiór danych zawiera informacje dotyczące populacji wybranych krajów i został opracowany na podstawie scalenia danych pozyskanych od kilku osób za pośrednictwem Google Sheets. Dane te obejmują podstawowe informacje demograficzne i fizyczne, takie jak: płeć (sex), rok przeprowadzenia badania pojedynczej osoby (year), wiek (age), wzrost (height) oraz waga (weight).

Na bazie tych podstawowych zmiennych możliwe było obliczenie dodatkowych wskaźników i uzupełnienie zbioru o nowe informacje. Wśród nich znajduje się między innymi wskaźnik masy ciała (BMI), poziom wykształcenia oparty na Międzynarodowej Standardowej Klasyfikacji Kształcenia (ISCED) – (education), a także obywatelstwo (citizenship).

Dzięki temu zestawowi danych możliwe jest prowadzenie różnorodnych analiz statystycznych i porównań między populacjami różnych krajów w kontekście cech demograficznych, edukacyjnych i zdrowotnych."
