# Installation du package nécessaire
install.packages("haven")
install.packages("dplyr")

install.packages("tinytex")
tinytex::install_tinytex()
tinytex::tlmgr_install("beamer")


# Chargement du package
library(haven)

# Revenus des ménages non issus de l'emploi (pension retraite, pension de veuvage, pension alimentaire, etc.)

data4 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s05_me_SEN2018.dta")
View(data2)

# Taille du dataframe

dim(data4) # 66120 observations et 19 variables

# Revenu annuel non issu de l'emploi

data4$revenu_non_emploi <- rowSums(data4[, c("s05q02", "s05q04", "s05q06", "s05q08", "s05q10", "s05q12", "s05q14")], na.rm = TRUE)

View(data4[, "revenu_non_emploi"])


# Création de la nouvelle base de données des revenus annuels individuels

revenu_annuel_individuel <- data3 %>%
  select(vague, grappe, menage, s01q00a, revenu_emploi) %>%
  distinct(vague, grappe, menage, s01q00a, .keep_all = TRUE) %>%  
  full_join(
    data4 %>% 
      select(vague, grappe, menage, s01q00a, revenu_non_emploi) %>%
      distinct(vague, grappe, menage, s01q00a, .keep_all = TRUE), 
    by = c("vague", "grappe", "menage", "s01q00a")
  ) 


View(revenu_annuel_individuel)

# Création de la nouvelle base de données des revenus annuels individuels

revenu_annuel_menage <- revenu_annuel_individuel %>%
  select(-s01q00a) %>% 
  group_by(vague, grappe, menage) %>%
  summarise(
    revenu_emploi_total = sum(revenu_emploi, na.rm = TRUE),
    revenu_non_emploi_total = sum(revenu_non_emploi, na.rm = TRUE),
    .groups = "drop" 
  )

View(revenu_annuel_menage)
