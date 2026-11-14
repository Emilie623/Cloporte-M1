#!/usr/bin/env Rscript
# Tests statistiques sur 100 simulations du modèle
# Le fichier simulations_100.txt doit se trouver dans le dossier de travail.

cat(strrep("=", 60), "\n")
cat("TESTS STATISTIQUES R\n")
cat(strrep("=", 60), "\n")

# ============================================================================
# Section 1: Charger les données
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 1: CHARGER LES DONNÉES\n")
cat(strrep("=", 60), "\n")

simulations <- read.table("simulations_100.txt", header=TRUE)
head(simulations)

# ============================================================================
# Section 2: Test du χ² pour une simulation
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 2: TEST DU Χ² POUR UNE SIMULATION\n")
cat(strrep("=", 60), "\n")

une_simulation <- simulations[1, ]
observes <- c(une_simulation$shelter1, une_simulation$shelter2)
print(observes)
chisq.test(observes)

# ============================================================================
# Section 3: Test binomial pour une simulation
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 3: TEST BINOMIAL POUR UNE SIMULATION\n")
cat(strrep("=", 60), "\n")

abri1 <- une_simulation$shelter1
abrites <- une_simulation$shelter1 + une_simulation$shelter2
print(binom.test(abri1, abrites, p=0.5))

# Une répartition 20-0
cat("2 × 0.5^20 =", signif(2 * 0.5^20, 4), "\n")
cat("Test binomial, 20-0 : p =", signif(binom.test(20, 20, p=0.5)$p.value, 4), "\n")
cat("Test du χ², 20-0 : p =", signif(chisq.test(c(20, 0))$p.value, 4), "\n")

# ============================================================================
# Section 4: Le test du χ² pour n simulations
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 4: LE TEST DU Χ² POUR N SIMULATIONS\n")
cat(strrep("=", 60), "\n")

n <- 100
p_valeurs <- c()
for (i in 1:n) {
  observes <- c(simulations$shelter1[i], simulations$shelter2[i])
  p_valeurs <- c(p_valeurs, chisq.test(observes)$p.value)
}

png("./r_pvaleurs_chi2.png", width=1200, height=750, res=150)
hist(p_valeurs, breaks=seq(0, 1, 0.05),
     col="steelblue", border="white",
     xlab="p-valeur", ylab="Nombre de simulations",
     main="")
abline(v=0.05, col="red", lty=2)
dev.off()
cat("Sauvegardé: r_pvaleurs_chi2.png\n")

cat("Proportion des p-valeurs inférieures à 0.05 :", mean(p_valeurs < 0.05), "\n")

# ============================================================================
# Section 5: Test binomial : quel abri l'emporte ?
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 5: TEST BINOMIAL : QUEL ABRI L'EMPORTE ?\n")
cat(strrep("=", 60), "\n")

choix_abri1 <- sum(simulations$shelter1 > simulations$shelter2)
choix_abri2 <- sum(simulations$shelter2 > simulations$shelter1)
cat("Abri 1 choisi", choix_abri1, "fois, abri 2 choisi", choix_abri2, "fois\n")
binom.test(choix_abri1, choix_abri1 + choix_abri2, p=0.5)

# ============================================================================
# Section 6: Test de Mann-Whitney : expériences et simulations
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 6: TEST DE MANN-WHITNEY : EXPÉRIENCES ET SIMULATIONS\n")
cat(strrep("=", 60), "\n")

experiences <- data.frame(
  shelter1 = c(14,  3, 12, 17,  6, 15,  2, 13,  5, 16),
  arena    = c( 2,  1,  3,  0,  2,  1,  3,  1,  0,  2),
  shelter2 = c( 4, 16,  5,  3, 12,  4, 15,  6, 15,  2)
)

set.seed(1)
n_tirees <- 10
tirees <- simulations[sample(nrow(simulations), n_tirees), ]

# Abri 1
cat("Abri 1, expériences :", experiences$shelter1, "\n")
cat("Abri 1, simulations :", tirees$shelter1, "\n")
print(wilcox.test(experiences$shelter1, tirees$shelter1, exact=FALSE))

# Abri le plus occupé
majorite_exp <- pmax(experiences$shelter1, experiences$shelter2)
majorite_sim <- pmax(tirees$shelter1, tirees$shelter2)
cat("Abri le plus occupé, expériences :", majorite_exp, "\n")
cat("Abri le plus occupé, simulations :", majorite_sim, "\n")
wilcox.test(majorite_exp, majorite_sim, exact=FALSE)

# ============================================================================
# Section 7: Test de Wilcoxon pour données appariées
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("SECTION 7: TEST DE WILCOXON POUR DONNÉES APPARIÉES\n")
cat(strrep("=", 60), "\n")

wilcox.test(simulations$shelter1, simulations$shelter2, paired=TRUE)

cat("\n", strrep("=", 60), "\n")
cat("SCRIPT R TERMINÉ\n")
cat(strrep("=", 60), "\n")

