## Datensatz erstellen 

# Altersvektor erstellen
set.seed(345)
Alter <- rnorm(100, 25, 2)

#Studienfachvektor erstellen
set.seed(348)
Studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"),
                      size = 100, prob = c(0.3,0.3, 0.15, 0.25), replace =TRUE)
table(Studienfach)

#Vektor 'Interesse an Mathematik' erstellen

set.seed(345)
InteresseMatheStats <- rbinom(length(which(Studienfach == "Statistik")), size = 6, prob = 0.6) +1
set.seed(345)
InteresseMatheMathe <- rbinom(length(which(Studienfach == "Mathe")), size = 6, prob = 0.85) +1
set.seed(345)
InteresseMatheInfo <- rbinom(length(which(Studienfach == "Informatik")), size = 6, prob = 0.4) +1
set.seed(345)
InteresseMatheData <- rbinom(length(which(Studienfach == "Data Science")), size = 6, prob = 0.25) +1


#Vektor 'Interesse an Programmieren' erstellen

set.seed(345)
InteressePrograStats <- rbinom(length(which(Studienfach == "Statistik")), size = 6, prob = 0.5) +1
set.seed(345)
InteressePrograMathe <- rbinom(length(which(Studienfach == "Mathe")), size = 6, prob = 0.3) +1
set.seed(345)
InteressePrograInfo <- rbinom(length(which(Studienfach == "Informatik")), size = 6, prob = 0.85) +1
set.seed(345)
InteressePrograData <- rbinom(length(which(Studienfach == "Data Science")), size = 6, prob = 0.7) +1

#Mathe-LK-Vektor erstellen 

set.seed(345)
MatheLKStats <- rbinom(length(which(Studienfach == "Statistik")), size =1 , prob = 0.6)
set.seed(345)
MatheLKMathe <- rbinom(length(which(Studienfach == "Mathe")), size = 1, prob = 0.85)
set.seed(345)
MatheLKInfo <- rbinom(length(which(Studienfach == "Informatik")), size = 1, prob = 0.4)
set.seed(345)
MatheLKData <- rbinom(length(which(Studienfach == "Data Science")), size = 1, prob = 0.25)

# Datensatz zusammensetzen

# Dataframe mit Alter, Studienfach
Studis <- data.frame(Alter = Alter, Studienfach = Studienfach)

# Mathe-Interesse je nach Studiengang zuweisen
Studis$MatheInteresse[Studienfach == "Statistik"] <- InteresseMatheStats
Studis$MatheInteresse[Studienfach == "Mathe"] <- InteresseMatheMathe
Studis$MatheInteresse[Studienfach == "Informatik"] <- InteresseMatheInfo
Studis$MatheInteresse[Studienfach == "Data Science"] <- InteresseMatheData

# Programmieren-Interesse je nach Studiengang zuweisen
Studis$PrograInteresse[Studienfach == "Statistik"] <- InteressePrograStats
Studis$PrograInteresse[Studienfach == "Mathe"] <- InteressePrograMathe
Studis$PrograInteresse[Studienfach == "Informatik"] <- InteressePrograInfo
Studis$PrograInteresse[Studienfach == "Data Science"] <- InteressePrograData

# Mathe-Lk je nach Studiengang zuweisen
Studis$MatheLK[Studienfach == "Statistik"] <- MatheLKStats
Studis$MatheLK[Studienfach == "Mathe"] <- MatheLKMathe
Studis$MatheLK[Studienfach == "Informatik"] <- MatheLKInfo
Studis$MatheLK[Studienfach == "Data Science"] <- MatheLKData

# factor() für Mathe-LK
Studis$MatheLK <- factor(Studis$MatheLK, labels = c("nein", "ja"))
# 1  kodiert für "nein", 2 für "ja"

#Struktur des Datensatzes ansehen
str(Studis)

#in csv-Datei
write.csv2(Studis, file = "~/DatensatzGitHub.csv")
