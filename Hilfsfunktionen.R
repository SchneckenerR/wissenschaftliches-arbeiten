## Hilfsfunktionen als interne Funktionen

## Berechnung der Schiefe nach drei Typen (Pearson, Yule-Pearson und Yule)

schiefe <- function(x){
    res <- (mean(x) - median(x))/sd(x)
    return(res)
    }

## Berechnung der Woelbung:

woelbung <- function(x){
  res <- 0
  n <- length(x)
  for(i in 1:n){
    res <- (x[i]-mean(x))^4 + res
  }
  res <- res/(sd(x)^4)
  res <- res/(n-1)
  res <- res - 3
  return(res)
}

