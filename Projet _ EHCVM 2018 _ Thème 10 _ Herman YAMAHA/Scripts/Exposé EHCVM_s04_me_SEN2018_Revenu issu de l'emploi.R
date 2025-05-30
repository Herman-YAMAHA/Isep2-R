# Installation du package nécessaire
install.packages("haven")
install.packages("dplyr")

# Chargement du package
library(haven)

# Revenus des ménages issus de l'emploi

data3 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s04_me_SEN2018.dta")
View(data3)

# Taille du dataframe

dim(data3) # 66120 observations et 93 variables

# Création des variables intermédiaires

data3$unite1 <- ifelse(data3$s04q43_unite == 1, 52, # semaine
                ifelse(data3$s04q43_unite == 2, 12, # mois
                ifelse(data3$s04q43_unite == 3, 4,  # trimestre
                ifelse(data3$s04q43_unite == 4, 1, NA))))  # année


data3$unite2 <- ifelse(data3$s04q45_unite == 1, 52,
                ifelse(data3$s04q45_unite == 2, 12,
                ifelse(data3$s04q45_unite == 3, 4,
                ifelse(data3$s04q45_unite == 4, 1, NA))))

data3$unite3 <- ifelse(data3$s04q47_unite == 1, 52,
                ifelse(data3$s04q47_unite == 2, 12,
                ifelse(data3$s04q47_unite == 3, 4,
                ifelse(data3$s04q47_unite == 4, 1, NA))))

data3$unite4 <- ifelse(data3$s04q49_unite == 1, 52,
               ifelse(data3$s04q49_unite == 2, 12,
               ifelse(data3$s04q49_unite == 3, 4,
               ifelse(data3$s04q49_unite == 4, 1, NA))))

data3$unite5 <- ifelse(data3$s04q58_unite == 1, 52,
               ifelse(data3$s04q58_unite == 2, 12,
               ifelse(data3$s04q58_unite == 3, 4,
               ifelse(data3$s04q58_unite == 4, 1, NA))))

data3$unite6 <- ifelse(data3$s04q60_unite == 1, 52,
               ifelse(data3$s04q60_unite == 2, 12,
               ifelse(data3$s04q60_unite == 3, 4,
               ifelse(data3$s04q60_unite == 4, 1, NA))))

data3$unite7 <- ifelse(data3$s04q62_unite == 1, 52,
              ifelse(data3$s04q62_unite == 2, 12,
              ifelse(data3$s04q62_unite == 3, 4,
              ifelse(data3$s04q62_unite == 4, 1, NA))))

data3$unite8 <- ifelse(data3$s04q64_unite == 1, 52,
              ifelse(data3$s04q64_unite == 2, 12,
              ifelse(data3$s04q64_unite == 3, 4,
              ifelse(data3$s04q64_unite == 4, 1, NA))))


# Revenu annuel issu de l'emploi

library(dplyr)

data3 <- data3 %>%
  mutate(revenu_emploi = coalesce(unite1, 0) * coalesce(s04q43, 0) +
           coalesce(unite2, 0) * coalesce(s04q45, 0) +
           coalesce(unite3, 0) * coalesce(s04q47, 0) +
           coalesce(unite4, 0) * coalesce(s04q49, 0) +
           coalesce(unite5, 0) * coalesce(s04q58, 0) +
           coalesce(unite6, 0) * coalesce(s04q60, 0) +
           coalesce(unite7, 0) * coalesce(s04q62, 0) +
           coalesce(unite8, 0) * coalesce(s04q64, 0))



