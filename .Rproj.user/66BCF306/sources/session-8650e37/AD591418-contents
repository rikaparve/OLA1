library(skimr)

################Opgave 1.1 – find data
#Data på ejendomme til salg i opgave 1 er fremkommet via webscapping af boligsiden.dk.
#Der ligger en csvfil i mappen med OLA-opgaven. Her er to billeder fra boligsiden.dk.
#Find de to rækker i csv-filen, som matcher de to huse.

boligsiden <- read.csv("data/boligsiden.csv")
tousvej106 <- which(boligsiden$vej == "tousvej")
boligsiden[71,]

egevej20 <- which(boligsiden$vej == "egevej") # 7 muligheder med vej navn, tiljøjer vejnr
egevej20 <- which(boligsiden$vej == "egevej" & boligsiden$vejnr == 20) 
boligsiden[2202,]

boligsiden_2_huse <- boligsiden[c(71,2202),]


################Opgave 1.2 - Vælg
#######Udvælg 2 ejendomme fra csv-filen og find dem på boligsiden.dk.
# no link, no date of scrapping, too much work to do it manually
# quick search of different listings shows that they have a pattern for the with postnummer and vejnavn, which we have
# so I do a code check
library(httr)
library(rvest)
library(XML)

startpage <- "https://www.boligsiden.dk/postnummer/8230/vej/tousvej/tilsalg"
rawres <- GET(url=startpage)
rawres$status_code
# 200

startpage2 <- "https://www.boligsiden.dk/postnummer/8230/vej/tousevej/tilsalg"
rawres2 <- GET(url=startpage2)
rawres2$status_code
# 404

# This means that the I have to ensure that these two columns (and others are clean)
boligsiden_df <- na.omit(boligsiden)
skim(boligsiden_df)

summary(boligsiden_df$postnr)
# Need to remove non-valid postcodes
lv_postnr <- boligsiden_df$postnr < 1000
boligsiden_df <- boligsiden_df[!lv_postnr,]
summary(boligsiden_df$postnr)
row.names(boligsiden_df) <- NULL

# Need to replace aa=å, oe=ø, ae=æ
#boligsiden_df$vej <- gsub("ae","æ", boligsiden_df$vej)
#boligsiden_df$vej <- gsub("oe","ø", boligsiden_df$vej)
#boligsiden_df$vej <- gsub("aa","å", boligsiden_df$vej)

### write a code to check the first line
startpage_loop <- paste0("https://www.boligsiden.dk/postnummer/",boligsiden_df$postnr[1],"/vej/", boligsiden_df$vej[1],"/tilsalg", collapse="")
rawres_loop <- GET(url=startpage_loop)
rawres_loop$status_code

#### write the loop
status_list <- c()
for (i in 1:100) {
  startpage_loop <- paste0("https://www.boligsiden.dk/postnummer/",boligsiden_df$postnr[i],"/vej/", boligsiden_df$vej[i],"/tilsalg", collapse="")
  rawres_loop <- GET(url=startpage_loop)
  status <- rawres_loop$status_code
  status_list <- rbind(status_list, status)
}
# doesn't work, because the links gets counted as a search page and comes back as active, it is only if there is a typo or no match between the street and postcode it gives 404
# https://www.boligsiden.dk/adresse/tousvej-5-8230-aabyhoej-07518710___5_______?udbud=9dc987ca-0a67-4404-923b-6e559f0f7b31
# Too much info in the individual listings link to be able to match them and check for status


#################Opgave 1.3 – Beskriv variabler
#Forklar NA-værdierne i csv-filen ud fra, hvad I har observeret i opgave 1.1 og 1.2.
#Derudover gør rede for variable I mener mangler i csv-filen sammenlignet med boligsiden.dk.
#(Hint: Hvordan vil I unikt identificere en bolig til salg via boligsiden?)







