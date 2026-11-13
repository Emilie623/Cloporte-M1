
# ============================================================================
# FONCTIONS PRINCIPALES
# ============================================================================

# Fonction: Entrée dans les abris
entree_abris <- function(N, A1, A2, mu) {
  entrees1 <- 0
  entrees2 <- 0
  dehors <- N - A1 - A2
  if (dehors > 0) {
    for (i in 1:dehors) {
      # Chaque individu tire séparément pour chaque abri
      choix1 <- runif(1) <= mu  # tirage pour l'abri 1
      choix2 <- runif(1) <= mu  # tirage pour l'abri 2
      
      if (choix1 && choix2) {
        # Si les deux tirages réussissent, on choisit au hasard
        if (runif(1) < 0.5) {
          entrees1 <- entrees1 + 1
        } else {
          entrees2 <- entrees2 + 1
        }
      } else if (choix1) {
        entrees1 <- entrees1 + 1
      } else if (choix2) {
        entrees2 <- entrees2 + 1
      }
      # sinon l'individu reste dehors
    }
  }
  return(c(entrees1, entrees2))
}

# Fonction: Sortie des abris
sortie_abris <- function(A1, A2, theta, xi) {
  sorties1 <- 0
  sorties2 <- 0
  if (A1 > 0) {
    for (i in 1:A1) {
      if (runif(1) <= theta * exp(-xi * A1)) {
        sorties1 <- sorties1 + 1
      }
    }
  }
  if (A2 > 0) {
    for (i in 1:A2) {
      if (runif(1) <= theta * exp(-xi * A2)) {
        sorties2 <- sorties2 + 1
      }
    }
  }
  return(c(sorties1, sorties2))
}

# Fonction: Dynamique temporelle
dynamique <- function(N=20, mu=0.2, theta=0.1, xi=0.15, T=10800) {
  A1 <- 0
  A2 <- 0
  abri1_histoire <- numeric(T)
  abri2_histoire <- numeric(T)
  for (t in 1:T) {
    e <- entree_abris(N, A1, A2, mu)
    s <- sortie_abris(A1, A2, theta, xi)
    A1 <- A1 + e[1] - s[1]
    A2 <- A2 + e[2] - s[2]
    abri1_histoire[t] <- A1 
    abri2_histoire[t] <- A2 
  }
  return(list(a1=abri1_histoire, a2=abri2_histoire))
}

# Fonction: Choix aléatoire
random_choice <- function(N=20, p=0.5) {
  Abri1 <- 0
  Abri2 <- 0
  for (i in 1:N) {
    if (runif(1) <= p) {
      Abri1 <- Abri1 + 1
    } else {
      Abri2 <- Abri2 + 1
    }
  }
  return(c(Abri1, Abri2))
}

# ============================================================================
# GÉNÉRATION DES FIGURES
# ============================================================================

# ----------------------------------------------------------------------------
# FIGURE 1: Evolution temporelle (une simulation)
# ----------------------------------------------------------------------------

res <- dynamique()
nbre_abrites1 <- res$a1
nbre_abrites2 <- res$a2

max_val <- max(c(nbre_abrites1, nbre_abrites2))

#png("figure_R_01_evolution_temporelle.png", width=800, height=500, res=120)
par(mar=c(5,4,4,8), xpd=TRUE)
plot(nbre_abrites1, type="l", col="blue", lwd=2,
     xlab="Temps (s)", ylab="Nombre d'individus abrités",
     ylim=c(0, max_val))
lines(nbre_abrites2, col="red", lwd=2)
legend(x=par("usr")[2]*1.05, y=par("usr")[4]*0.9, 
       legend=c("Abri 1", "Abri 2"),
       col=c("blue", "red"), lty=1, lwd=2)
#dev.off()

cat("Résultat final: Abri 1 =", nbre_abrites1[length(nbre_abrites1)], 
    ", Abri 2 =", nbre_abrites2[length(nbre_abrites2)], "\n")

# ----------------------------------------------------------------------------
# FIGURE 2: Histogramme de 400 simulations
# ----------------------------------------------------------------------------

n_sim <- 400
aa1 <- numeric(n_sim) 
aa2 <- numeric(n_sim)

for (i in 1:n_sim) {
  res <- dynamique()
  aa1[i] <- res$a1[length(res$a1)]
  aa2[i] <- res$a2[length(res$a2)]
}

max_val <- max(c(table(aa1), table(aa2)))

#png("figure_R_02_histogramme_400.png", width=800, height=500, res=120)
par(mar=c(5,4,4,8), xpd=TRUE)
hist(aa1, breaks=seq(-0.5, 20.5, 1),
     col=rgb(0, 0, 1, 0.5),
     xlab="Nombre d'individus", ylab="Fréquence",
     main="", ylim=c(0, max_val))
hist(aa2, breaks=seq(-0.5, 20.5, 1),
     col=rgb(1, 0, 0, 0.5),
     add=TRUE)
legend(x=par("usr")[2]*1.05, y=par("usr")[4]*0.9,
       legend=c("Abri 1", "Abri 2"),
       fill=c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
#dev.off()

cat(sprintf("Moyenne Abri 1: %.2f ± SD: %.2f\n", mean(aa1), sd(aa1)))
cat(sprintf("Moyenne Abri 2: %.2f ± SD: %.2f\n", mean(aa2), sd(aa2)))





  


n_sim <- 400
aa1 <- numeric(n_sim) 
aa2 <- numeric(n_sim)
Abri_gagnant <- numeric(n_sim) ##

for (i in 1:n_sim) {
  res <- dynamique()
  aa1[i] <- res$a1[length(res$a1)]
  aa2[i] <- res$a2[length(res$a2)]
  Abri_gagnant[i] <- max(aa1[i], aa2[i])
}

max_val <- max(c(table(aa1), table(aa2)))

#png("figure_R_02_histogramme_400.png", width=800, height=500, res=120)
par(mar=c(5,4,4,8), xpd=TRUE)
hist(Abri_gagnant, breaks=seq(-0.5, 20.5, 1),
     col=rgb(0, 0, 1, 0.5),
     xlab="Nombre d'individus", ylab="Fréquence",
     main="", ylim=c(0, max_val))


# ----------------------------------------------------------------------------
# FIGURE 3: Choix aléatoire - 10000 simulations
# ----------------------------------------------------------------------------

aa3 <- numeric(10000)
for (i in 1:10000) {
  res <- random_choice(20, 0.5)
  aa3[i] <- res[1]
}

#png("figure_R_03_random_choice.png", width=800, height=500, res=120)
par(mar=c(5,4,4,8), xpd=TRUE)
hist(aa3, breaks=seq(-0.5, 20.5, 1),
     col="steelblue", border="white",
     xlab="Nombre d'individus", ylab="Fréquence",
     main="")
legend(x=par("usr")[2]*1.05, y=par("usr")[4]*0.9,
       legend="Abri 1", fill="steelblue")
#dev.off()

cat(sprintf("Moyenne: %.2f ± SD: %.2f\n", mean(aa3), sd(aa3)))

# ----------------------------------------------------------------------------
# FIGURE 4: Distribution binomiale théorique
# ----------------------------------------------------------------------------

N <- 20
p <- 0.5
outcomes <- 0:N
binom_probs <- dbinom(outcomes, N, p)

#png("figure_R_04_binomiale.png", width=800, height=500, res=120)
par(mar=c(5,4,4,8), xpd=TRUE)
barplot(binom_probs * 10000, names.arg=outcomes,
        col="steelblue", border="white",
        xlab="Nombre d'individus", ylab="Fréquence (sur 10000)")
#dev.off()

# ----------------------------------------------------------------------------
# FIGURE 5: Comparaison simulation vs théorie
# ----------------------------------------------------------------------------

x_vals <- 0:20
y_vals <- dbinom(x_vals, 20, 0.5) * 10000
max_val <- max(c(table(aa3), y_vals))

#png("figure_R_05_comparison.png", width=800, height=500, res=120)
par(mar=c(5,4,4,8), xpd=TRUE)
hist(aa3, breaks=seq(-0.5, 20.5, 1),
     col=adjustcolor("steelblue", alpha.f=0.6), border="white",
     xlab="Nombre d'individus", ylab="Fréquence",
     main="", ylim=c(0, max_val))
# Superposer la courbe théorique
lines(x_vals, y_vals, type="o", col="red", lwd=2, pch=19)
legend("topright",
       legend=c("Simulation", "Théorie binomiale"),
       col=c(adjustcolor("steelblue", alpha.f=0.6), "red"),
       pch=c(15, 19), pt.cex=c(2, 1), lty=c(NA, 1), lwd=c(NA, 2))
#dev.off()


# ============================================================================
# STATISTIQUES RÉCAPITULATIVES
# ============================================================================
# Le test du chi-carré compare le choix aléatoire (figures 3 et 5) à la
# distribution binomiale (figure 4) : c'est le choix aléatoire qui est
# compatible avec la binomiale, pas la dynamique (figure 2), où les individus
# se regroupent dans un même abri.
# Comme tout test au seuil de 5 %, il conclut parfois à tort à une différence.

cat("\n")
cat(strrep("=", 60), "\n")
cat("STATISTIQUES RÉCAPITULATIVES\n")
cat(strrep("=", 60), "\n\n")

cat("400 simulations de la dynamique:\n")
cat(sprintf("  Abri 1: moyenne = %.2f, écart-type = %.2f\n", mean(aa1), sd(aa1)))
cat(sprintf("  Abri 2: moyenne = %.2f, écart-type = %.2f\n", mean(aa2), sd(aa2)))
cat("\n")

cat("10000 simulations avec choix aléatoire:\n")
cat(sprintf("  Abri 1: moyenne = %.2f, écart-type = %.2f\n", mean(aa3), sd(aa3)))
cat(sprintf("  Théorie binomiale: moyenne = %.1f, écart-type = %.2f\n", N*p, sqrt(N*p*(1-p))))
cat("\n")

cat("Test de chi-carré (choix aléatoire vs théorie binomiale):\n")
obs_freq <- table(factor(aa3, levels=0:20))
exp_prob <- dbinom(0:20, 20, 0.5)
test <- chisq.test(obs_freq, p=exp_prob)
cat(sprintf("  Statistique chi-carré: %.2f\n", test$statistic))
cat(sprintf("  p-value: %.4f\n", test$p.value))
if (test$p.value > 0.05) {
  cat("  Conclusion: Le choix aléatoire suit bien la distribution binomiale!\n")
} else {
  cat("  Conclusion: Différence significative détectée.\n")
}
cat("\n")
cat(strrep("=", 60), "\n")


