# Funktionen Skript 1
# Pakete:
##install.packages("DescTools")
##install.packages("tidyverse")

## L: falls man die Pakete noch runterladen muss, ist hier Befehl auskommentiert

library("DescTools") # fuer Zusammenhangskoeff.

# a) Funktion berechnet verschiedene geeignete deskriptive Statistiken fuer metrische Variablen
# und gibt diese aus
summary_metrisch <- function(x){
  # Lagemasse: Minimum, 1. Quartil, Median, Mittelwert, 3. Quartil, Maximum
  min <- min(x)
  quart1 <- quantile(x,type=2, probs=0.25)
  median <- median(x)
  mean <- mean(x)
  quart3 <- quantile(x,type=2,probs=0.75)
  max <- max(x)
  # Streuungsmasse
  span <- max-min
  iqr <- quart3-quart1
  sd <- sd(x)
  # Schiefe- und Woelbungsmasse
  schiefe <- schiefe(x) #Hilfsfunktion
  woelbung <- woelbung(x) #Hilfsfunktion
  
  # Visualisiere die Daten in einem Histogramm:
  hist(x, freq=FALSE, ylab="Häufigkeitsdichte", main="Histogramm")
  
  # Fasse alles in einer Matrix zusammen und bennene ihre Zeilen und Spalten
  summary_metr <- matrix(round(c(min,quart1,median,mean,quart3,max,span,iqr,sd,schiefe,woelbung),digits=2),
                        nrow=11,byrow = TRUE)
  colnames(summary_metr) <- ""
  rownames(summary_metr) <- c("Min","1.Quart","Median","Mean","3.Quart","Max","Span","IQR","SD","Skew.","Kurt.")
  # Gebe diese Matrix aus
  return(summary_metr)
} 



# b) Funktion berechnet versch. geeignete deskriptive Statistiken fuer kategoriale Variablen
# und gibt diese aus
summary_kategorisch <- function(x){
  # absolute und relative Haufigkeiten:
  absHF <- table(x)
  relHF <- absHF/length(x)
  
  # Visualisierung in einem Balkendiagramm:
  barplot(absHF, main="Balkendiagramm", ylab="Anzahl")
  
  # Fasse alles in einer Matrix zusammen und bennene ihre Zeilen und Spalten
  ## L: Spaltenanzahl entspricht individueller Tabellenlaenge
  summary_kat <- matrix(c(absHF, relHF), ncol=length(absHF), nrow=2, byrow=TRUE)
  colnames(summary_kat) <- dimnames(absHF)[[1]]
  rownames(summary_kat) <- c("abs. HF", "rel. HK")
  # Gebe diese Matrix aus
  return(summary_kat)
}



# c) Funktion berechnet versch. geeign. desk. bivariate Statistiken fuer den Zusammenhang
# zwischen zwei kategorialen Variablen und gibt diese aus
# Eingabe: die beiden Vektoren x,y der beiden kategorialen Variablen
zsh_zwei_kateg <- function(x,y) {
  tab <- table(x,y)
 
  # Pearsons Kontingenzkoeffizient (je kleiner der Koeff. umso geringer der Zsh zw beiden Merkmalen)
  pcont <- ContCoef(tab)
  
  # korrigierter Pearsons Kontingenzkoeffizient (normiert: Werte zw 0 und 1, wobei 0 bedeutet
  # dass kein stat. Zsh besteht und 1 perfekter stat. Zsh)
  pcont_kor <- ContCoef(tab, correct=TRUE)
  
  # Cramers V Koeffizient (0 bedeutet kein stat. Zsh, 1 bedeutet perfekter stat. Zsh)
  cram <- CramerV(tab)
  
  summary <- matrix(c(pcont,pcont_kor,cram), nrow=3, ncol=1)
  colnames(summary) <- ""
  ## L: "phi" rausgenommen, da Formel dazu fehlt
  rownames(summary) <- c("Pears. Kontingenzkoeff.", "korrigierter Pears. Kontkoeff.",
                         "Cramers V")
  return(summary)
}



# d) Funktion berechnet versch. geeign. desk. bivariate Statistiken fuer den Zusammenhang
# zwischen einer dichotomen und einer metrischen Variable und gibt diese aus
# Eingabe: Die beiden Vektoren, wobei x metrisch und y dichotom ist. Zusaetzlich kann
# angegeben werden, ob y in der Form ("ja", "nein") ist. Dann sollte der Paramter 
# y_n gleich TRUE gesetzt werden. 
zsh_metr_dich <- function(x, y, y_n = FALSE) {
  
  # Das  Alter muss gerundet werden fuer Cramers V
  x = round(x)  
  
  # Abfrage zur Umwandlung von "ja"-"nein" zu 1-0 in y 
  if(y_n == TRUE){
       y[y == "ja"] = 1
       y[y == "nein"] = 0
       y = as.numeric(y)
  }
  
  tab <- table(x,y)

  print(tab)
  # Cramers V Koeff (passe Skalenniveau der metrischen Variable an die niedrigere dichotome an)
  cram <- CramerV(tab)
  
  # Korrelationskoeff nach Bravais Pearson (passe an hoeheres Skalenniveau an, also dichotome Var. als metrisch ansehen)
  bpcor <- cor(x,y)
  
  # Mittelwerte fuer jeweils beide Auspraegungen der dichotomen Variable ausgeben:
  mean0 <- mean(x[which(y == 0)])
  mean1 <- mean(x[which(y == 1)])
  
  ## L: Absatz durch "\n" , damit Angaben getrennt sind
  cat("Cramers V Koeffizient:",cram,"\n")
  cat("Bravais-Pearson Korrelationskoeff:",bpcor,"\n")
  cat("Mittelwert mit 0 Auspraegung:",mean0,"\n")
  cat("Mittelwert mit 1 Auspraegung:",mean1,"\n")
  
}

# f)

VisKat <- function(x,y,z){
  #Mosaikplot aufgeteilt nach variable x (im besten Fall dichotome Variable),
  #in Abh?ngigkeit von y und z
  op <- par(oma = c(1,5,1,1))
  mosaicplot(data = data, ~x+y+z)
  mtext(side =3, text = "z",line =1)
  par(op) 
}



