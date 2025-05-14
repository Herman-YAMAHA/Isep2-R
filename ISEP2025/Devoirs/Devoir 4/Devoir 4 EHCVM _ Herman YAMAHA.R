# Installation du package nécessaire
install.packages("haven")

# Chargement du package
library(haven)

# 1 - Taille et composition des ménages

# → Analyser la taille moyenne des ménages et la structure par âge/genre.

donnees1 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s01_me_SEN2018.dta")
View(donnees1)

library(dplyr)

donnees1 <- donnees1 %>%
  mutate(id_menage = paste(grappe, menage, sep = "_"))

# Afficher la nouvelle colonne
print(donnees1$id_menage)

# Calcul et affichage de la taille moyenne des ménages
library(dplyr)

# Calcul du nombre total d'individus
total_individus <- nrow(donnees1)

# Calcul du nombre total de ménages
total_menages <- donnees1 %>%
  distinct(id_menage) %>%
  nrow()

# Taille moyenne des ménages
taille_moyenne <- total_individus / total_menages

# Affichage
cat("La taille moyenne des ménages est de :", round(taille_moyenne, 2), "\n")
# Resultat : La taille moyenne des ménages est de : 9.24 

# Structure des ménages par sexe

library(dplyr)

# Étape 1 : Nombre d'hommes et de femmes par ménage
structure_par_menage <- donnees1 %>%
  group_by(id_menage, s01q01) %>%
  summarise(nombre = n(), .groups = "drop")

# Étape 2 : Moyenne par sexe sur tous les ménages
moyenne_par_sexe <- structure_par_menage %>%
  group_by(s01q01) %>%
  summarise(moyenne_par_menage = mean(nombre))

# Affichage
print(moyenne_par_sexe)

#s01q01           moyenne_par_menage
#<dbl+lbl>                     <dbl>
#  1     1 [Masculin]               4.44
#2     2 [Féminin]                5.18
#3 NA(a)                          1 

# Structure des ménages par age

donnees1 <- donnees1 %>%
  mutate(
    age = 2025 - s01q03c
  )

# Créer les classes d’âge simples (si pas encore faites)
donnees1 <- donnees1 %>%
  mutate(classe_age_simple = case_when(
    age >= 0 & age <= 14 ~ "Enfant",
    age >= 15 & age <= 24 ~ "Jeune",
    age >= 25 & age <= 59 ~ "Adulte",
    age >= 60            ~ "Senior",
    TRUE                 ~ NA_character_
  ))

# Nombre d'individus par classe d'âge et par ménage
age_par_menage <- donnees1 %>%
  group_by(id_menage, classe_age_simple) %>%
  summarise(n = n(), .groups = "drop")

# Moyenne par classe d’âge sur tous les ménages
moyenne_age_par_menage <- age_par_menage %>%
  group_by(classe_age_simple) %>%
  summarise(moyenne = mean(n))

# Affichage
print(moyenne_age_par_menage)



# 2) Taux d'alphabétisation
# → Calculer le pourcentage d’individus alphabétisés (3 ans et plus).

donnees2 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/Projet EHCVM/SEN2018_menage/s02_me_SEN2018.dta")
View(donnees2)

var_labels <- sapply(donnees2, attr, "label")

# Mettre sous forme de data frame
df_labels <- data.frame(
  variable = names(var_labels),
  label = unname(var_labels),
  row.names = NULL,
  stringsAsFactors = FALSE
)

print(df_labels)


library(dplyr)

# Créer la variable alphabete_plus_3 directement dans donnees2
library(dplyr)

# Si age n'est pas encore dans donnees2, tu peux l'ajouter ici
donnees2 <- donnees2 %>%
  mutate(
    alphabete_plus_3 = ifelse(
      # Vérifier si au moins une des variables de lecture ou d'écriture est égale à 1
      s02q01__1 == 1 | s02q01__2 == 1 | s02q01__3 == 1 | s02q02__1 == 1 | s02q02__2 == 1, 
      1, 0
    )
  )

# Affichage pour vérifier le résultat
head(donnees2)



# Affichage des premières lignes pour vérifier
head(donnees1)

