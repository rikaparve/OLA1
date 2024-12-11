###################Opgave 2.1 – Beskrivende statistik
#Lav beskrivende statistik for data på boliger til salg via boligsiden.dk fra opgave 1.
#Vær opmærksom på, hvilke variable I mener kan betragtes som x’er og y i en række af simpel lineære regressioner.

# summary - Min.,1st Qu.,Median,Mean,3rd Qu.,Max.
# Standard deviation
# Plot
library(stringr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)

#### Pris
summary(boligsiden_df$pris)
boligsiden_df$pris <- as.numeric(gsub("[^0-9]", "",boligsiden_df$pris))
boxplot(boligsiden_df$pris)
lv_pris <- boligsiden_df$pris > 7000000
boligsiden_df <- boligsiden_df[!lv_pris,]
sd_pris <- sd(boligsiden_df$pris)


#### kvmpris - lots of problems converting the values to numeric without .
summary(boligsiden_df$kvmpris)
boligsiden_df$kvmpris <- boligsiden_df$kvmpris *1000
boxplot(boligsiden_df$kvmpris)
lv_kvmpris <- boligsiden_df$kvmpris > 38000
boligsiden_df <- boligsiden_df[!lv_kvmpris,]
sd_kvmpris <- sd(boligsiden_df$kvmpris)


#### størrelse
summary(boligsiden_df$størrelse)
boxplot(boligsiden_df$størrelse)
lv_stl <- boligsiden_df$størrelse < 50 | boligsiden_df$størrelse > 250
boligsiden_df <- boligsiden_df[!lv_stl,]
sd_stl <- sd(boligsiden_df$størrelse)


#### mdudg
summary(boligsiden_df$mdudg)
boligsiden_df$mdudg <-  boligsiden_df$mdudg * 1000
boxplot(boligsiden_df$mdudg)
lv_mdudg <- boligsiden_df$mdudg > 8000
boligsiden_df <- boligsiden_df[!lv_mdudg,]
sd_mdudg <- sd(boligsiden_df$mdudg)


#### grund
summary(boligsiden_df$grund)

boligsiden_df <- boligsiden_df %>%
  mutate(
    extracted_value = ifelse(
      !is.na(str_extract(grund, "^\\d{3}")),   # Check if extraction is not NA
      str_extract(grund, "^\\d{3}"),           # Extract first 3 digits
      ifelse(
        !is.na(str_extract(grund, "^\\d{2}")),   # Check if extraction is not NA
        str_extract(grund, "^\\d{2}"),           # Extract first 3 digits
        grund * 1000                             # Keep the original value but remove the decimal
    )
  ))
boligsiden_df$grund <- as.numeric(boligsiden_df$extracted_value)
boligsiden_df <- boligsiden_df[,-c(13)]
boxplot(boligsiden_df$grund)
lv_grund <- boligsiden_df$grund > 1600 | boligsiden_df$grund < 150
boligsiden_df <- boligsiden_df[!lv_grund,]
sd_grund <- sd(boligsiden_df$grund)


#### liggetid
boligsiden_df$liggetid <- as.numeric(str_extract(boligsiden_df$liggetid, "^\\d+"))
summary(boligsiden_df$liggetid)
sd_tid <- sd(boligsiden_df$liggetid)

# opført
boligsiden_df$alder <- 2024 - boligsiden_df$opført
summary(boligsiden_df$alder)
sd_alder <- sd(boligsiden_df$alder)


###################Opgave 2.2 - Korrelation
#Hvad er korrelationen mellem m2 og prisen for boliger lagt på boligsiden.dk?
cor_m2_pris <- cor(data.frame(kvm=boligsiden_df$størrelse,
                    pris=boligsiden_df$pris))
melted_cor_m2_pris <- melt(cor_m2_pris)

ggplot(melted_cor_m2_pris, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(x = "", y = "", title="Kvm og pris er positivt korrelaret") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size= 15)) +
  theme(axis.text.y = element_text(size= 15))

###################Opgave 2.3 – Simple regressioner
#Lav minimum 5 simple regressioner mellem pris pr. m2 og 5 andre variable i csv-filen fra opgave 1.
#Giv en forklaring på, hvilken af de fem modeller, der bedst forklarer pris pr. m2.
#Derudover skal I understøtte jeres 5 modeller med et bud på om, der er sammenhæng (korrelation) mellem jeres 5 udvalgte variable (hint: lav en korrelationsmatrix).
model_mdudg <- lm(kvmpris ~ mdudg, data=boligsiden_df)
summary(model_mdudg)

model_grund <- lm(kvmpris ~ grund, data=boligsiden_df)
summary(model_grund)

model_liggetid <- lm(kvmpris ~ liggetid, data=boligsiden_df)
summary(model_liggetid)

model_alder <- lm(kvmpris ~ alder, data=boligsiden_df)
summary(model_alder)


### correlation calculations
cor_boligsiden <- cor(data.frame(
  kvmpris=boligsiden_df$kvmpris,
  mdudg=boligsiden_df$mdudg,
  grund=boligsiden_df$grund,
  liggetid=boligsiden_df$liggetid,
  alder=boligsiden_df$alder))

melted_cor_boligsiden <- melt(cor_boligsiden)

ggplot(melted_cor_boligsiden, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 10) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(axis.text.y = element_text(size= 15)) +
  theme(axis.text.x = element_text(size= 15))


##### 
saveRDS(boligsiden_df,"Boligsiden_renset.rds")




