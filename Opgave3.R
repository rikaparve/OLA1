#########################Opgave 3.1
# Slå med 25000 terninger
roll <- function(n){
  die <- 1:6
  dice <- sample(die, size=n, replace=TRUE)
  return(dice)
}

roll_25K <- roll(25000)

# Hvor mange 5'ere har terning slået?
freqterninger <- as.data.frame(table(roll_25K))

#Hvad er sandsynligheden, givet jeres resultatet med de 25.000 terninger, for at jeres script slår en 5'er?
freqterninger$`Sansynlighed i %` <- freqterninger$Freq/sum(freqterninger$Freq)*100
colnames(freqterninger) <- c("Die", "Frekvens", "Sansynlighed i %")


#####################Opgave 3.2
# Slå med 6 terninger og vis summen.
roll_sum <- function(n){
  die <- 1:6
  dice <- sample(die, size=n, replace=TRUE)
  sum(dice)
}

roll_sum(6)
#[1] 24

# Slå 10.000 gange med 6 terninger og lav et barplot
roll_10K <- replicate(10000,roll_sum(6))

library(ggplot2)
freq_10K <- table(roll_10K)
barplot(height=freq_10K, xlab="Summer", ylab="Antal af kast", main = "10.000 kast med 6 terninger skaber normalfordeling")



######################Opgave 3.3
# Slå 1.000.000 gange med 6 terninger
roll_1M <- replicate(1000000,roll_sum(6))

# Lav en barplot og sammenlign med plotten fra 3.2
freq_1M <- table(roll_1M)
barplot(height=freq_1M, xlab="Summer", ylab="Antal af kast", main = "1.000.000 kast med 6 terninger skaber normalfordeling")


###################Opgave 3.4
# Lav et script, der viser en tilfældigt opstillet række af tallene 1,2,3,5,6
v1 <- sample(c(1:3,5,6),5, replace=F)

# Lav en matrix med to kolonner og fem rækker, hvor den første kolonne skal være tallene 2 til 6
# og den anden kolonne skal være jeres tilfældige række af tallene (burg cbind)

mtal <- matrix(cbind(2:6,v1),nrow=5, ncol=2, byrow=F)






