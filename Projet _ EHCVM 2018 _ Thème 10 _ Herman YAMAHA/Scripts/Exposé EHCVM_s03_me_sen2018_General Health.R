# Installation des packages nécessaires
install.packages("haven")
install.packages("tidyr")

# Chargement des packages
library(haven)
library(tidyr)

data2 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s03_me_SEN2018.dta")
View(data2)

# Création de la variable des depenses annuelles de santé hors assurance
data2 <- data2 %>%
  mutate(depense_sante_hors_assurance =
           replace_na(s03q13, 0) * 4 +
           replace_na(s03q14, 0) * 4 +
           replace_na(s03q15, 0) * 4 +
           replace_na(s03q16, 0) * 4 +
           replace_na(s03q17, 0) * 4 +
           replace_na(s03q18, 0) * 4 +
           replace_na(s03q24, 0) +
           replace_na(s03q26, 0) +
           replace_na(s03q27, 0) +
           replace_na(s03q29, 0) +
           replace_na(s03q30, 0) +
           replace_na(s03q31, 0))

# Création de la variable des depenses annuelles de santé avec assurance inclue
data2 <- data2 %>%
  mutate(depense_sante_avec_assurance = depense_sante_hors_assurance - 
           (replace_na(s03q33, 0) * depense_sante_hors_assurance) / 100)

# Agréger les dépenses par ménage (en tenant compte de l'assurance)
depenses_menage <- data2 %>%
  group_by(grappe, menage) %>%
  summarise(depense_sante_menage = sum(depense_sante_avec_assurance, na.rm = TRUE), .groups = "drop")

View(depenses_menage)

# Enregistrer le dataframe
write_dta(depenses_menage, "depenses_menage.dta")

