# Installation du package nécessaire
install.packages("haven")

# Chargement du package
library(haven)

# Transferts reçus

data3 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s13a_2_me_sen2018.dta")
View(data3)

# Taille du dataframe

dim(data3) # 8403 observations et 19 variables

# Création des variables intermédiaires

data3$frequence <- ifelse(data3$s13aq17b == 1, 12,
                   ifelse(data3$s13aq17b == 2, 4,
                   ifelse(data3$s13aq17b == 3, 2,
                   ifelse(data3$s13aq17b == 4, 1,
                   ifelse(data3$s13aq17b == 5, 0, NA))))) # Transfert irrégulier


# Transfert annuel régulier

library(dplyr)

data3 <- data3 %>%
  mutate(Transfert_annuel_regulier = coalesce(frequence, 0) * coalesce(s13aq17a, 0))
  
# Transfert annuel régulier par ménage

data3 <- data3 %>%
  group_by(vague, grappe, menage) %>%
  mutate(transfert_annuel_regulier_menage = sum(Transfert_annuel_regulier, na.rm = TRUE)) %>%
  ungroup()

# Insertion de la variable 'transfert_annuel_regulier_menage' dans la base 'revenu_annuel_menage'

# Étape 1 : Créer une table avec les transferts réguliers par ménage (en ne gardant que la première occurrence)
transferts_reguliers_menage <- data3 %>%
  group_by(vague, grappe, menage) %>%
  summarise(transfert_annuel_regulier_menage = first(transfert_annuel_regulier_menage), .groups = "drop")

# Étape 2 : Fusionner avec revenu_annuel_menage
revenu_annuel_menage <- revenu_annuel_menage %>%
  left_join(transferts_reguliers_menage, by = c("vague", "grappe", "menage")) %>%
  mutate(transfert_annuel_regulier_menage = coalesce(transfert_annuel_regulier_menage, 0))

View(revenu_annuel_menage)