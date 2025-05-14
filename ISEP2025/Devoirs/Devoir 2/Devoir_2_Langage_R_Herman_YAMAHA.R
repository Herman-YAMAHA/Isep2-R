
## 1. Fonctions utilisant if, for, foreach, while

# Exemple de fonction avec if, for, while
maFonction <- function(x) {
  if (x %% 2 == 0) {
    print("Nombre pair")
  } else {
    print("Nombre impair")
  }
  
  for (i in 1:x) {
    print(i)
  }
  
  i <- 1
  while (i <= x) {
    print(paste("while:", i))
    i <- i + 1
  }
}


# 2. Recherche de nombres premiers

estPremier <- function(n) {
  if (n < 2) return(FALSE)
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

# 3. Résoudre une équation simple (ex: 2x + 3 = 7)

resoudreEquation <- function(a, b, c) {
  # ax + b = c
  x <- (c - b) / a
  return(x)
}

# 4. Résoudre un système d’équations

resoudreSysteme <- function() {
  A <- matrix(c(2, 3, 1, -1), nrow=2)
  B <- c(5, -1)
  sol <- solve(A, B)
  return(sol)
}

# 6. Inverser une matrice

inverserMatrice <- function(M) {
  if (det(M) == 0) {
    return("La matrice n'est pas inversible")
  }
  return(solve(M))
}


### Applications

maFonction(9)

# which
vec <- c(3, 7, 1, 9)
which(vec > 5)  # retourne les positions

estPremier(56)

resoudreEquation(4,3,2)

# Définir une matrice carrée 2x2
matrice <- matrix(c(2, 1, 3, 4), nrow = 2, byrow = TRUE)

# Appel de la fonction
inverse <- inverserMatrice(matrice)

# Affichage du résultat
print("Matrice d'origine :")
print(matrice)

print("Inverse de la matrice :")
print(inverse)


### Optimisation

optimisation <- function() {
  f <- function(x) (x[1]^2 + x[2]^2 + x[1]*x[2])
  result <- optim(c(2, 2), f, method="BFGS")
  return(result$par)
}

minimum <- optimisation()
print("Point où le minimum est atteint :")
print(minimum)


### Etude de cas primaire - Aider à resoudre les 04 opérations

operationPrimaire <- function(a, b) {
  return(list(
    addition = a + b,
    soustraction = a - b,
    multiplication = a * b,
    division = if (b != 0) a / b else "Division par zéro"
  ))
}

# Appel de la fonction avec les nombres 10 et 2
resultat <- operationPrimaire(10, 2)

# Affichage des résultats
print("Résultats des opérations :")
print(resultat)

