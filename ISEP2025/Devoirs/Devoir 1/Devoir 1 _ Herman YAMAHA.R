
# Ici commentaire

## Creation de vecteurs

x <- c(1:7) # Vecteur d'entiers
typeof(x)

y <- as.character(x) # Vecteur de charactères
print(y)

x1 <- c(x,NA,9)
print(x1)

sum(x1)
sum(x1,na.rm = TRUE)

x2 <- c("A", 1, 2, "Maman")
print(x2)

x3 <- c("A", 1, 2.25, TRUE)
print(x3)

x <- 1:10  # Vecteur de 1 à 10
y <- seq(1, 10, by=2)  # Vecteur de 1 à 10 avec un pas de 2
print(y)

s <- c(TRUE, 1, 2)
print(s)


## Creation de matrices

mat <- matrix(1:9, nrow = 3, ncol = 3)
print(mat)

## Creation de dataframe

df <- data.frame(  Nom = c("Alice", "Bob", "Charlie"),
                   Age = c(25, 30, 22),
                   Score = c(90, 85, 88))
print(df)

df$Nom   # Accède à la colonne "Nom"
df[1, ]  # Accède à la première ligne

## Creation de liste

my_list <- list(  Nom = "Herman",
                  Notes = c(15, 18, 20),
                  Matrice = matrix(1:4, nrow = 2))

my_list$Nom    # Récupère "Herman"
my_list[[2]]   # Récupère le vecteur des notes

##  Les objets

ls()  # Voir la liste des objets
rm(x)  # Supprime l'objet x

## Creation de facteurs

sexe <- factor(c("Homme", "Femme", "Homme", "Femme", "Enfant"))
print(sexe)
levels(sexe)  # Affiche les catégories ("Femme", "Homme")

## Jeu de données

data(iris)  # Charge le dataset iris
head(iris)  # Affiche les premières lignes
summary(iris)  # Résumé statistique


## Cours du 04/03/2025

x_complex <- c(1+2i,3+4i)
print(x_complex)

typeof(x_complex)

ma_liste <- list (42,"Bonjour",TRUE,c(1,2,3))
print(ma_liste)

ma_liste2 <- list (nombre = 42,texte = "Bonjour",logique = TRUE,vecteur=c(1,2,3))
print(ma_liste2)

print(ma_liste2[[2]])

ma_liste2$vecteur

## Exercice : Création d'un dataframe avec 10 observations

df <- data.frame(
  
  ID = 1:10,  # Identifiant unique
  
  Nom = c("Alice", "Bob", "Charlie", "David", "Eva", "Frank", "Grace", "Hugo", "Iris", "Jack"),
  
  Age = sample(20:30, 10, replace = TRUE),  # Âge entre 20 et 30 ans
  
  Sexe = factor(c("H", "F", "H", "H", "F", "H", "F", "H", "F", "H")),  # Variable catégorielle
  
  Salaire = round(runif(10, 1500, 5000), 2),  # Salaire aléatoire entre 1500 et 5000
  
  Expérience = sample(0:10, 10, replace = TRUE),  # Années d'expérience
  
  Diplôme = factor(c("Licence", "Master", "Doctorat", "Master", "Licence",
                     "Doctorat", "Licence", "Master", "Licence", "Doctorat")),
  
  Ville = factor(c("Dakar", "Yaoundé", "Abidjan", "Paris", "Bamako", 
                   "Lomé", "Cotonou", "Nairobi", "Ouaga", "Kinshasa")),
  
  Note = round(rnorm(10, mean = 12, sd = 2), 1),  # Note sur 20 (moyenne 12, écart-type 2)
  
  Statut = factor(ifelse(runif(10) > 0.5, "Actif", "Inactif"))  # Statut aléatoire
)

print(df)


## Cours

# Création d'un tableau à plusieurs dimensions

# Array (tableau à 3 dimensions)
mon_array <- array(1:24, dim = c(2, 3, 4))
print(mon_array)

## Fin

## Cours

# Création d'un vecteur 
vect <- c(1,5,7,6)
print(vect)

x_raw <- raw(5)
print(x_raw)

## Création d'une liste

Herman <-list(
  Nom='YAMAHA',
  Prénom='Herman',
  classe='ISEP2',
  Note=c(10,12,13)
)

print(Herman)
print(Herman[1])

## Création de facteur

x_ord<-factor(c("petit","moyen","grand","petit"),
              levels= c("petit","moyen","grand"),
              ordered=TRUE)
print(x_ord)

## Création de séquence

# Séquence régulière

ss = seq(1,10,by=2)
print(ss)

# Séquence de longueur spécifique

seq(0,2,length.out =5)

# Répétition

rep(1:7,times=3)

rep(1:3,each=2)

# Fonction statistique de base

donnees<-c(20,15,21,18,24,19,22, 25,65,25,89)

# Statistiquesdescriptives

mean(donnees) #Moyenne

median(donnees) #Médiane

sd(donnees) #Écart-type

var(donnees) #Variance