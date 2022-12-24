# Funktionen Skript 1
# Pakete:
library("moments") # fuer Woelbungs- und Schiefemasse
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
  schiefe <- skewness(x)
  woelbung <- kurtosis(x) - 3
  
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
  
  # Fasse alles in einer Matrix zusammen und bennene ihre Zeilen und Spalten
  summary_kat <- matrix(c(absHF, relHF), ncol=4, nrow=2, byrow=TRUE)
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
  rownames(summary) <- c("Phi", "Pears. Kontingenzkoeff.", "korrigierter Pears. Kontkoeff.",
                         "Cramers V")
  return(summary)
}



# d) Funktion berechnet versch. geeign. desk. bivariate Statistiken fuer den Zusammenhang
# zwischen einer dichotomen und einer metrischen Variable und gibt diese aus
# Eingabe: die beiden Vektoren, wobei x metrisch und y dichotom ist
zsh_metr_dich <- function(x,y) {
  tab <- table(x,y)
  
  # Cramers V Koeff (passe Skalenniveau der metrischen Variable an die niedrigere dichotome an)
  cram <- CramerV(tab)
  
  # Korrelationskoeff nach Bravais Pearson (passe an hoeheres Skalenniveau an, also dichotome Var. als metrisch ansehen)
  bpcor <- cor(x,y)
  
  cat("Cramers V Koeffizient:",cram)
  cat("Bravais-Pearson Korrelationskoeff:",bpcor)
  
  # Eventuell noch andere Koeffizienten?
}






