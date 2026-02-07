# Modelowanie Claim Severity (GLM)

## Opis projektu
Projekt poświęcony jest budowie i porównaniu zaawansowanych modeli statystycznych do prognozowania średniej wartości pojedynczej szkody komunikacyjnej (*claim severity*). Analiza została przeprowadzona na zbiorze danych ubezpieczeniowych `freMPL5` (pakiet `CASdatasets`).

## Wykorzystane zmienne
Model objaśnia wysokość odszkodowania (`ClaimAmount`) za pomocą następujących charakterystyk:
* **DrivAge**: Wiek kierowcy.
* **LicAge**: Staż posiadania prawa jazdy.
* **Gender**: Płeć.
* **RiskArea**: Strefa ryzyka (poziomy 1-13).
* **PastClaims**: Historia liczby szkód kierowcy.

## Zastosowane modele statystyczne
W ramach projektu zaimplementowano i przetestowano cztery modele:
1. **Podstawowy model Gamma GLM**: Model bazowy dla danych o rozkładzie ciągłym, dodatnim i asymetrycznym.
2. **Model Gamma z interakcjami**: Rozszerzenie modelu bazowego o zależności między zmiennymi objaśniającymi.
3. **Double GLM (DGLM)**: Zaawansowane podejście pozwalające na jednoczesne modelowanie wartości oczekiwanej (średniej) oraz dyspersji (wariancji).
4. **Model Tweediego (p=2.6)**: Model wykorzystujący rozkład z wykładniczej rodziny dyspersyjnej, optymalizowany pod kątem parametru mocy $p$.

## Metody porównawcze i walidacja
Do oceny jakości predykcyjnej oraz dopasowania modeli wykorzystano następujące metryki i narzędzia:

### Kryteria statystyczne
* **Kryterium informacyjne Akaike (AIC)**: Użyte do rankingu modeli pod kątem dopasowania przy jednoczesnym karaniu za nadmierną liczbę parametrów.
    * *Wynik*: Model Tweediego uzyskał najniższą wartość AIC (25 099.36).

### Analiza diagnostyczna
* **Wykresy kwantylowe (Q-Q Plots)**: Weryfikacja założeń dotyczących rozkładu reszt (Gamma vs Inverse Gaussian).
* **Analiza funkcji wariancji**: Sprawdzenie relacji $V(\mu) \propto \mu^p$.

### Weryfikacja biznesowa (Zdolność segmentacyjna)
* **Lift Plot**: Porównanie średniej odpowiedzi rzeczywistej z predykcją modelu w grupach decylowych.
* **Lift Ratio**: Wyliczenie współczynnika siły segmentacji.
    * *Wynik*: Model Tweediego wykazał najwyższy współczynnik **Lift Ratio = 3.69**, co potwierdza jego najwyższą użyteczność biznesową w procesie taryfikacji.

## Struktura plików
* `Modele_GLM.R`: Główny skrypt zawierający proces czyszczenia danych, estymację modeli oraz obliczenia AIC.
* `Funkcje_pomocnicze.R`: Autorskie funkcje do dekompozycji wyników, rysowania krzywych Lorenza i walidacji modeli.
* `Prezentacja_doubleGLM.pdf`: Szczegółowe podsumowanie wyników analizy wraz z wizualizacjami.
