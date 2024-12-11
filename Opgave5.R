################## Opgave 5
###### Opgave 5.1
# Lav en 36 x 3 dataframe med kolonnenavne ”Klasse”,”Uge”,”Score”.
# Første kolonne skal fyldes med A,B,C,D så der startes med 9 A’er, derpå 9 B’er osv.
# Anden kolonne skal fyldes med tallene 1 til 9, der gentages for hvert bogstav. 
# Sidste kolonne skal fyldes med observationer. Det er op til jer, hvilke værdier I vil putte i framen. (hint: benyt R-funktionen seq())

klasse <- rep(c("A","B","C", "D"), times=1, length.out=36, each=9)
uge <- rep(seq(1,9), times=4, length.out=36, each=1)
score <- sample(1:100, 36, replace=T)

df <- data.frame(
  Klasse=klasse,
  Uge=uge,
  Score=as.numeric(score)
)



####### Opgave 5.2
#I skal lave en ny dataframe, der er 9x3 og bygger på den dataframe I lavede opgave 5.1.
#I skal tage udgangspunkt i framen fra opgave 5.1 og loope igennem.
#I loopet skal I hver tredje gang lave en dataframe 1x3 dataframe med samme navne som i 5.1. (Hint: brug modulo-operatoren til at ramme hver tredje). 
#Indholdet skal være som følger: 1 element og 2 element henter I fra 5.1-framen.
#Det sidste element skal være gennemsnittet af de forrige tre observationer.

# Create a new empty dataframe
df2 <- df[0,]

# Define row index value
rowindex=1

# Create the loop with modulo to grab every third line
for (i in 1:nrow(df)) {
  if(i %% 3 == 1) {
    df2[rowindex,] = df[i,]
    rowindex=rowindex+1
  }
}

# Add the average of scores
df2$Score <- NA

for (i in seq(1, nrow(df), by=3)) {
  if (i + 2 <= nrow(df)) {
    score1 = df$Score[i]
    score2 = df$Score[i+1]
    score3 = df$Score[i+2]
    meanscore = mean(c(score1, score2, score3), na.rm = TRUE)
    rowindex_df2 = (i %/% 3) + 1
    df2$Score[rowindex_df2] = meanscore
  }
}

df2$Score <- round(df2$Score, digits = 1)

################## Opgave 5.3
library(tidyverse)

df_pivoted <- df2 %>%
  pivot_wider(names_from = Klasse, values_from = Score)

              