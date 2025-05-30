# Installation du package nécessaire
install.packages("haven")

# Chargement du package
library(haven)

data1 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s02_me_SEN2018.dta")
View(data1)

# Le nombre de valeurs manquantes dans la variable s02q03 : études dans une école formelle
sum(is.na(data1$s02q03)) # 5633

# Le nombre de valeurs manquantes dans la variable s02q05 : études dans une école non formelle
sum(is.na(data1$s02q05)) # 33577

library(dplyr)

# S'assurer que s02q03 est de type caractère
data1$s02q03 <- as.character(data1$s02q03)

# Imputation de la variable s02q03 par modalité majoritaire dans chaque ménage
data1 <- data1 %>%
  group_by(grappe, menage) %>%
  mutate(
    modalite_majoritaire = {
      freq <- table(s02q03)
      if (length(freq) > 0) names(freq)[which.max(freq)] else NA_character_
    },
    s02q03 = ifelse(is.na(s02q03), modalite_majoritaire, s02q03)
  ) %>%
  ungroup() %>%
  select(-modalite_majoritaire)

# Le nombre de valeurs manquantes dans la variable s02q03 : études dans une école formelle
sum(is.na(data1$s02q03)) # 03

# S'assurer que s02q05 est de type caractère
data1$s02q05 <- as.character(data1$s02q05)

# Imputation de s02q05 par modalité majoritaire dans chaque ménage
data1 <- data1 %>%
  group_by(grappe, menage) %>%
  mutate(
    modalite_majoritaire = {
      freq <- table(s02q05)
      if (length(freq) > 0) names(freq)[which.max(freq)] else NA_character_
    },
    s02q03 = ifelse(is.na(s02q05), modalite_majoritaire, s02q05)
  ) %>%
  ungroup() %>%
  select(-modalite_majoritaire)

# Le nombre de valeurs manquantes dans la variable s02q05 : études dans une école non formelle
sum(is.na(data1$s02q05)) # 33577

# Création de l'indicatrice des individus qui sont allés à l'école
data1 <- data1 %>%
  mutate(indicatrice_ecole = if_else(s02q05 == 1 | s02q03 == 1, 1, 0))

# Le nombre de valeurs manquantes dans la variable indicatrice_ecole 
sum(is.na(data1$indicatrice_ecole)) # 23128

# S'assurer que indicatrice_ecole est de type caractère
#data1$indicatrice_ecole <- as.character(data1$indicatrice_ecole)

# Imputation de indicatrice_ecole par modalité majoritaire dans chaque ménage

# Étape 1 : calcul de la modalité majoritaire par ménage
modalite_majoritaire_par_menage <- data1 %>%
  group_by(grappe, menage) %>%
  summarise(
    modalite_majoritaire = {
      vec <- indicatrice_ecole[!is.na(indicatrice_ecole)]
      if (length(vec) == 0) NA else names(sort(table(vec), decreasing = TRUE))[1]
    },
    .groups = "drop"
  )

# Étape 2 : jointure pour associer à chaque individu la modalité majoritaire de son ménage
data1 <- data1 %>%
  left_join(modalite_majoritaire_par_menage, by = c("grappe", "menage")) %>%
  mutate(indicatrice_ecole = ifelse(is.na(indicatrice_ecole), modalite_majoritaire, indicatrice_ecole)) %>%
  select(-modalite_majoritaire)


# Le nombre de valeurs manquantes dans la variable indicatrice_ecole 
sum(is.na(data1$indicatrice_ecole)) # 4227

# Les observations où indicatrice_ecole est manquantes
manquants <- data1 %>%
  filter(is.na(indicatrice_ecole)) %>%
  select(grappe, menage, s01q00a)

# Nombre de ménages avec des indicatrices d'école manquantes
nb_menages <- manquants %>%
  distinct(grappe, menage) %>%
  nrow()

cat("Nombre de ménages avec des indicatrices d'école manquantes :", nb_menages) # 883


# Suppression des lignes avec les observations de indicatrice_menage manquantes
data1 <- data1 %>% 
  filter(!is.na(indicatrice_ecole))

dim(data1)

# Création de la base de données data_menage
data_menage <- data1 %>%
  group_by(grappe, menage) %>%
  summarise(
    nb_individus = n(),
    nb_individus_etudes = sum(indicatrice_ecole == 1, na.rm = TRUE),
    .groups = "drop"
  )

dim(data_menage)

View(data_menage)

# Enregistrer le dataframe
write_dta(data_menage, "data_menage.dta")
