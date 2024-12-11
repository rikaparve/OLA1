library(ggplot2)
library(reshape2)
library(dplyr)
library(corrplot)
library(dkstat)
library(tidyr)

###################### Opgave 4
########### Opgave 4.1
# Hent data

alkohol_meta <- dst_meta(table = "FU02", lang = "da")
alkohol_meta$values

my_query <- list(
  KONSUMGRP = c("02.1.1.1 Spiritus og likør","02.1.1.2 Alkoholiske læskedrikke","02.1.2.1 Vin af druer",
                  "02.1.2.2 Vin af andre frugter","02.1.2.3 Hedvin","02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                  "02.1.3.1 Pilsnerøl, guldøl","02.1.3.2 Andre alkoholholdige øl","02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl","02.1.3.4 Øl-baserede drikkevarer"),
  PRISENHED = c("Faste priser"),
  Tid = "*"
)

alkohol_df <- dst_get_data(table = "FU02", query = my_query, lang = "da")

# Data transformation
alkohol_df <- alkohol_df[,-2]

# Pivot the data to wide format
alkohol_wide <- alkohol_df %>%
  pivot_wider(names_from = KONSUMGRP, values_from = value) %>%
  arrange(TID)
colnames(alkohol_wide) <- c("År","Spiritus_og_likor", "Alkoholiske_laeskedrikke", "Vin_af_druer", "Vin_af_andre_frugter", "Hedvin","Vinbaserede_drikkevarer_og_alkoholfri_vin",
                        "Pilsner_ol_guld_ol", "Andre_alkoholholdige_ol", "Ol_med_lavt_alkoholindhold_og_alkoholfri_ol","Ol_baserede_drikkevarer")
alkohol_wide <- alkohol_wide[-c(1:6),]

# Create afunction for normalisation
norm <- function(x){
  vnorm <- ((x-min(x))/(max(x)-min(x)))
  return(vnorm)
}

#Create a copy of the data frame for normalized values
alkohol_norm <- alkohol_wide

# Loop through each column (excluding the first one with years)
for (col in names(alkohol_norm)[-1]) {
  alkohol_norm[[col]] <- norm(alkohol_norm[[col]])
}

# Plot the beers
ggplot(alkohol_norm, aes(x = År)) + 
  geom_line(aes(y = Pilsner_ol_guld_ol, color = "Pilsner_ol_guld_ol"), linewidth = 1) + 
  geom_line(aes(y = Andre_alkoholholdige_ol, color = "Andre_alkoholholdige_ol"), linewidth = 1) + 
  geom_line(aes(y = Ol_baserede_drikkevarer, color = "Ol_baserede_drikkevarer"), linewidth = 1) + 
  theme_minimal(base_size=13) + 
  labs(title = "Udvikling i forbruget af forskellige typer af øl", x = "År", y = "Normaliseret forbrug i faste priser", color = "Typer af øl")


# Plot the wines
ggplot(alkohol_norm, aes(x = År)) + 
  geom_line(aes(y = Vin_af_druer, color = "Vin_af_druer"), linewidth = 1) + 
  geom_line(aes(y = Vin_af_andre_frugter, color = "Vin_af_andre_frugter"), linewidth = 1) + 
  geom_line(aes(y = Hedvin, color = "Hedvin"), linewidth = 1) + 
  theme_minimal(base_size=13) + 
  labs(title = "Udvikling i forbruget af forskellige typer af vin", x = "År", y = "Normaliseret forbrug i faste priser", color = "Typer af vin")

# Plot the alcoholfree
ggplot(alkohol_norm, aes(x = År)) + 
  geom_line(aes(y = Vinbaserede_drikkevarer_og_alkoholfri_vin, color = "Vinbaserede_drikkevarer_og_alkoholfri_vin"), linewidth = 1) + 
  geom_line(aes(y = Ol_med_lavt_alkoholindhold_og_alkoholfri_ol, color = "Ol_med_lavt_alkoholindhold_og_alkoholfri_ol"), linewidth = 1) + 
  theme_minimal(base_size=13) + 
  labs(title = "Udvikling i forbruget af alkoholfri vin og øl", x = "År", y = "Normaliseret forbrug i faste priser", color = "Typer")

# Plot the others
ggplot(alkohol_norm, aes(x = År)) + 
  geom_line(aes(y = Spiritus_og_likor, color = "Spiritus_og_likor"), linewidth = 1) + 
  geom_line(aes(y = Alkoholiske_laeskedrikke, color = "Alkoholiske_laeskedrikke"), linewidth = 1) + 
  theme_minimal(base_size=13) + 
  labs(title = "Udvikling i forbruget af andre typer af alkohol", x = "År", y = "Normaliseret forbrug i faste priser", color = "Typer")


###############Opgave 4.2 Lav en korrelationsmatrix over forbrugsgrupperne under 02.1 og konkludér på resultaterne

###### Create a correlation matrix for all the groups
# Melt the correlation matrix for ggplot2
cor_mat_alkohol <- cor(alkohol_wide[,2:11])
melted_cor_matrix <- melt(cor_mat_alkohol)


# Create a heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=15)) +
  theme(axis.text.y = element_text(size= 15))

####### Make a correlation matrix for the groups from lines
alkohol_wide$vin <- alkohol_wide$Vin_af_druer+alkohol_wide$Vin_af_andre_frugter+alkohol_wide$Hedvin
alkohol_wide$ol <- alkohol_wide$Pilsner_ol_guld_ol+alkohol_wide$Andre_alkoholholdige_ol+alkohol_wide$Ol_baserede_drikkevarer
alkohol_wide$alkoholfri <- alkohol_wide$Vinbaserede_drikkevarer_og_alkoholfri_vin + alkohol_wide$Ol_med_lavt_alkoholindhold_og_alkoholfri_ol
alkohol_wide$andet <- alkohol_wide$Spiritus_og_likor + alkohol_wide$Alkoholiske_laeskedrikke

# Melt the correlation matrix for ggplot2
cor_mat_groups <- cor(alkohol_wide[,12:15])
melted_cor_matrix_groups <- melt(cor_mat_groups)


# Create a heatmap
ggplot(melted_cor_matrix_groups, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=15)) +
  theme(axis.text.y = element_text(size= 15))
