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
                   ifelse(data3$s13aq17b == 5, 1, NA))))) # Transfert irrégulier


# Transfert total

library(dplyr)

data3 <- data3 %>%
  mutate(Transfert_total = coalesce(frequence, 0) * coalesce(s13aq17a, 0))
  
# Transfert total par ménage

data3 <- data3 %>%
  group_by(vague, grappe, menage) %>%
  mutate(transfert_total_menage = sum(Transfert_total, na.rm = TRUE)) %>%
  ungroup()

# Insertion de la variable 'transfert_total_menage' dans la base 'revenu_annuel_menage'

# Étape 1 : Créer une table avec les transferts par ménage (en ne gardant que la première occurrence)
transferts_menage <- data3 %>%
  group_by(vague, grappe, menage) %>%
  summarise(transfert_total_menage = first(transfert_total_menage), .groups = "drop")

# Étape 2 : Fusionner avec revenu_annuel_menage
revenu_annuel_menage <- revenu_annuel_menage %>%
  left_join(transferts_menage, by = c("vague", "grappe", "menage")) %>%
  mutate(transfert_total_menage = coalesce(transfert_total_menage, 0))

View(revenu_annuel_menage)


# Calcul du revenu total au cours des 12 derniers mois

revenu_annuel_menage <- revenu_annuel_menage %>%
  mutate(
    revenu_total = coalesce(revenu_emploi_total, 0) +
      coalesce(revenu_non_emploi_total, 0) +
      coalesce(transfert_total_menage, 0)
  )

# Part des tranferts dans le revenu total

revenu_annuel_menage <- revenu_annuel_menage %>%
mutate(part_transferts_dans_revenu_total = round(100 * transfert_total_menage / 
                                            revenu_total, 2))

sum(is.na(revenu_annuel_menage$part_transferts_dans_revenu_total)) # 1500 ménages sans aucun revenu.


# Renommer la variable 'part_transferts_dans_revenu_total' pour la rendre compatible avec Stata
revenu_annuel_menage <- revenu_annuel_menage %>%
  rename(
    part_transferts_revenu_total = part_transferts_dans_revenu_total
  )

# Exporter le data frame en format .dta
write_dta(revenu_annuel_menage, "revenu_12_derniers_mois_menage_EHCVM.dta")

