#!/usr/bin/env Rscript
# Script R avec tous les exemples du document

cat(strrep("=", 60), "\n")
cat("EXEMPLES R\n")
cat(strrep("=", 60), "\n")

# ============================================================================
# Section 1: Boucles for
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 1: BOUCLES FOR\n")
cat(strrep("=", 60), "\n")

cat("\n--- Exemple 1: Itérer sur un vecteur ---\n")
fruits <- c("pastèques", "abricots", "fraises")
for (fruit in fruits) {
  print(fruit)
}

cat("\n--- Exemple 2: Utiliser : ---\n")
N <- 5
for (i in 0:N) {
  x <- 9.87 * i
  print(paste(i, x))
}

cat("\n--- Exemple 3: Garder en mémoire ---\n")
x <- c()
for (i in 0:N) {
  x <- c(x, 9.87 * i)
}
print(x)

# ============================================================================
# Section 2: Conditions if
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 2: CONDITIONS IF\n")
cat(strrep("=", 60), "\n")

cat("\n--- Exemple 1: Simple if ---\n")
x <- 10
if (x > 5) {
  print("x est supérieur à 5")
}

cat("\n--- Exemple 2: if/else if/else ---\n")
x <- 3
if (x > 5) {
  print("x est supérieur à 5")
} else if (x == 5) {
  print("x est égal à 5")
} else {
  print("x est inférieur à 5")
}

# ============================================================================
# Section 3: Fonctions
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 3: FONCTIONS\n")
cat(strrep("=", 60), "\n")

cat("\n--- Exemple 1: Fonction simple ---\n")
saluer <- function(nom) {
  print(paste("Bonjour,", nom, "!"))
}

saluer("Laura")
saluer("Patrick")

cat("\n--- Exemple 2: Fonction avec return ---\n")
surface_rectangle <- function(longueur, largeur) {
  surface <- longueur * largeur
  return(surface)
}

surface <- surface_rectangle(5, 3)
cat(paste("La surface du rectangle est de", surface, "unités carrées\n"))

cat("\n--- Exemple 3: Paramètres par défaut ---\n")
surface_cercle <- function(rayon=3) {
  surface <- pi * rayon^2
  return(surface)
}

surface <- surface_cercle()
cat(paste("La surface du cercle est de", surface, "unités carrées\n"))

surface <- surface_cercle(5)
cat(paste("La surface du cercle est de", surface, "unités carrées\n"))

# ============================================================================
# Section 4: Exemple combiné
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 4: EXEMPLE COMBINÉ\n")
cat(strrep("=", 60), "\n")

cat("\n--- Exemple 1: Pile ou face ---\n")
pile_ou_face <- function(nombre_de_lances) {
  nombre_de_piles <- 0
  nombre_de_faces <- 0
  for (i in 1:nombre_de_lances) {
    if (runif(1) <= 0.5) {
      nombre_de_piles <- nombre_de_piles + 1
    } else {
      nombre_de_faces <- nombre_de_faces + 1
    }
  }
  return(c(nombre_de_piles, nombre_de_faces))
}

lances <- 1000000
resultat <- pile_ou_face(lances)
piles <- resultat[1]
faces <- resultat[2]
cat(paste("Sur", lances, "lancés, nous avons eu", piles, "piles et", faces, "faces\n"))

# ============================================================================
# Section 5: Figures
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 5: GÉNÉRATION DES FIGURES\n")
cat(strrep("=", 60), "\n")

# Figure 1: Série temporelle
cat("\n--- Figure 1: Série temporelle ---\n")
temps <- seq(0, 10, length.out=10001)
variable_x <- 10 * exp(-temps)

png("./r_serie_temporelle.png", width=600, height=475, res=150)
plot(temps, variable_x, type="l", col="black",
     xlab="Temps", ylab="Variable")
dev.off()
cat("Sauvegardé: r_serie_temporelle.png\n")

# Figure 2: Histogramme
cat("\n--- Figure 2: Histogramme ---\n")
set.seed(42)
insectes <- sample(0:20, 1000, replace=TRUE)

png("./r_histogramme.png", width=600, height=475, res=150)
hist(insectes, breaks=seq(-0.5, 20.5, 1),
     col="pink", border="white",
     xlab="Nombre d'insectes abrités", ylab="Fréquence",
     main="")
dev.off()
cat("Sauvegardé: r_histogramme.png\n")

cat("\n", strrep("=", 60), "\n")
cat("SCRIPT R TERMINÉ\n")
cat(strrep("=", 60), "\n")
