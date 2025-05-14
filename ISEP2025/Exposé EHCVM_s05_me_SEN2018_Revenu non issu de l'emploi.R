# Installation du package nécessaire
install.packages("haven")

# Chargement du package
library(haven)

# Revenus des ménages issus de l'emploi

data2 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s05_me_SEN2018.dta")
View(data2)

# Taille du dataframe

dim(data2) # 66120 observations et 19 variables

# Revenu annuel non issu de l'emploi

data2$revenu_non_emploi <- rowSums(data2[, c("s05q02", "s05q04", "s05q06", "s05q08", "s05q10", "s05q12", "s05q14")], na.rm = TRUE)

View(data2[, "revenu_non_emploi"])


# Création de la nouvelle base de données des revenus annuels individuels

library(dplyr)

revenu_annuel_individuel <- data1 %>%
  select(vague, grappe, menage, revenu_emploi) %>%
  full_join(
    data2 %>% select(vague, grappe, menage, revenu_non_emploi),
    by = c("vague", "grappe", "menage")
  ) %>%
  mutate(revenu_total = rowSums(across(c(revenu_emploi, revenu_non_emploi)), na.rm = TRUE))

View(revenu_annuel_individuel)
