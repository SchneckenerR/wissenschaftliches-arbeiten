# Analyse des Datensatzes mittels der Funktionen aus Aufgabe 3

# Pakete:
library(readxl)

# Daten einlesen: (ACHTUNG: Pfad bei euch anpassen)
#setwd("C:/Users/Admin/Documents/GitHub/wissenschaftliches-arbeiten")
Studenten = read.csv("DatensatzGitHub.csv", sep=";", dec=",", header=TRUE)[,2:6]

#Einbetten der Funktionen aus 'funktionen1.R' und der Hilfsfunktionen aus 'Hilfsfunktionen.R'
source("funktionen1.R")
source("Hilfsfunktionen.R")

str(Studenten)
#'data.frame':	100 obs. of  5 variables:
#  $ Alter          : num  23.4 24.4 24.7 24.4 24.9 ...
#$ Studienfach    : chr  "Statistik" "Mathe" "Informatik" "Informatik" ...
#$ MatheInteresse : int  6 7 2 3 3 5 2 4 2 2 ...
#$ PrograInteresse: int  3 2 7 7 6 3 6 6 6 6 ...
#$ MatheLK        : chr  "ja" "ja" "nein" "nein" ...

# Es gibt 100 Beobachtungen und 5 Merkmale
# Alter ist eine metrische stetige Variable
# Studienfach ist eine kategorische Variable
# MatheInteresse und PrograInteresse sind ordinale Variablen
# MatheLK ist eome dichotome / kategorische Variable

# Einbinden der Funktionen auf den Skripten funktionen1.R und funktionen2.R

### Univariate Analyse

# Analyse des Alters:
summary_metrisch(Studenten$Alter)
#Min     19.84
#1.Quart 23.45
#Median  24.50
#Mean    24.85
#3.Quart 26.30
#Max     30.64
#Span    10.80
#IQR      2.85
#SD       1.96
#Skew.    0.18
#Kurt.   -0.04

# Am Histogramm lässt sich eine ungefaehre Normalvertielung des Alters beobachten.
# Die Verteilung ist in etwa symmetrisch und eher platt als schief, also platykurtisch.
# Zu den Kennzahlen:
# Das Alter der Studenten ist im Durchschnitt 24,85 Jahre und die Standardabweichung betraegt
# 1,96 Jahre. Dies ist auch am Histogramm gut ersichtlich.
# Die Schiefe ist nahe bei 0 mit einem Wert von 0,18. Dies erklaert die fast symmetrische
# Schiefe der Verteilung.
# Die Wölbung ist auch fast 0 mit -0,4, sodass anhand der Kennzahl auf eine ausgewogene Verteilung
# zu schließen ist.
# Das Minimum betreagt 19,84 Jahre und  das Maximum 30,64.


# Analyse des Studienfachs:
summary_kategorisch(Studenten$Studienfach)
#         Data Science Informatik Mathe Statistik
#abs. HF        34.00      24.00 12.00      30.0
#rel. HK         0.34       0.24  0.12       0.3

# Anhang des Balkendiagramms sieht man, dass es am meisten Data Scientisten gibt (34),
# danach kommen Statistiker mit 30 Studenten, Informatiker mit 24 Studenten und
# am wenigsten vorhanden sind Mathematiker mit 12 Studenten.
# Das laesst vermuten, dass es in etwa je 3/10 Data Scientisten, Statistiker und
# Informatiker gibt und 1/10 Mathematiker.


# Analyse des Interesses an Mathe: (Lennart)
# hier die Funktion d) fuer ordinale Daten verwenden


# Analyse des Interesses am Programmieren: (Lennart)
# hier die Funktion d) fuer ordinale Daten verwenden


# Analyse von MatheLK
summary_kategorisch(Studenten$MatheLK)
#         ja nein
#abs. HF 50.0 50.0
#rel. HK  0.5  0.5

# Anhand des Balkendiagramms sieht man, dass genau die Haelfte der Studenten
# einen Mathe LK im Abitur belegt haben und die andere Haelfte nicht.
# Deshalb wird vermutet, dass die Verteilung der Mathe LK Variable zufaellig auf
# die Studenten im Datensatz aufgeteilt wurde, also mit gleicher Wahrscheinlichkeit p=0.5.



### bivariate Analyse

# Analyse des Zusammenhangs zwischen Mathe LK und Studienfach: (Lennart)
# hier Funktion c) verwenden


# Analyse des Zusammenhangs zwischen Mathe LK und Interesse an Mathe:
zsh_zwei_kateg(Studenten$MatheInteresse, Studenten$MatheLK)
#Pears. Kontingenzkoeff.        0.6459485
#korrigierter Pears. Kontkoeff. 0.9135091
#Cramers V                      0.8461678

# Alle drei Koeffizienten sind deutlich ueber 0,5, sodass von einem definitiv vorhandenen
# und staerkeren Zusammenhang zwischen Mathe LK und der Angabe, dass das Interesse an
# Mathe hoch ist, besteht. Der korrigierte Peasrs. Kontkoeff, der ja normiert ist, nimmt
# den groessten Wert der drei Koeffizienten an mit 0,91, also ein sehr starker Zusammenhang.
# Auch Cramers V ist sehr gross mit 0,84. Etwas kleiner aber trotzdem groesser als 0,5 ist
# der nicht normierte Pears. Kontkoeff. mit 0,65.

# => Insgesamt ein starker Zusammenhang zwischen Mathe LK und MatheInteresse.



# Analyse des Zusammenhangs zwischen Mathe LK und Interesse an Programmieren: (Lennart)
# hier Funktion c) verwenden


# Analyse des Zusammenhangs zwischen Interesse an Mathe und Intreresse am Programmieren: (Lennart)
# hier Funktion c) verwenden



# Analyse des Zusammenhangs zwischen Alter und Mathe LK:
zsh_metr_dich(Studenten$Alter, Studenten$MatheLK)
# die Funktion hat ein Error, weil im Datensatz Mathe LK als "ja", "nein"
# kodiert ist, wahrend man fuer die Funktion zsh_metr_dich braucht, dass die dichotome V.
# als 0-1 Variable kodiert ist.
# TO DO: da muesste man in Zeile 93 "if(y!=is.numeric(y)){y <- as.numeric(y)}" abaendern



# Analyse des Zusammenhangs zwischen Mathe LK, MatheInteresse und PrograInteresse (Lennart)
# Funktion f) hier verwenden

