tinytex::uninstall_tinytex()
tinytex::install_tinytex()


# Installation du package nécessaire
install.packages("haven")
install.packages("dplyr")
install.packages("skimr")
install.packages("psych")
install.packages("moments")
install.packages("tidyr")
install.packages("ggplot2")

# Chargement du package
library(haven)

data6 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/ISEP2025/revenu_12_derniers_mois_menage_EHCVM.dta")
data7 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/ISEP2025/data_menage.dta")
data8 <- read_dta("C:/Users/T Informatique/Desktop/Langage R/ISEP2025/depenses_menage.dta")

View(data6)
View(data7)
View(data8)
View(data_menage_final)

# Fusion successive des trois bases
data_menage_final <- data6 %>%
  inner_join(data7, by = c("grappe", "menage")) %>%
  inner_join(data8, by = c("grappe", "menage"))

data_menage_final <- data_menage_final %>%
  mutate(ratio_etude = 100* nb_individus_etudes / nb_individus)

# Relation entre transferts reçus et éducation

# Visualisation

install.packages("ggpubr")

library(ggplot2)
library(ggpubr)

# Modèle de régression
modele <- lm(transfert_total_menage ~ ratio_etude, data = data_menage_final)

# Extraire les paramètres
a <- round(coef(modele)[1], 2)
b <- round(coef(modele)[2], 2)
r2 <- round(summary(modele)$r.squared, 3)
p <- round(summary(modele)$coefficients[2, 4], 3)

# Créer l'équation formatée en texte simple (sans parse = TRUE)
eq_text <- paste0("y = ", b, "x + ", a,
                  "   |   R² = ", r2,
                  "   |   p = ", p)

# Graphique
ggplot(data_menage_final, aes(x = ratio_etude, y = transfert_total_menage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", 
           x = 0.1, 
           y = 9500000, 
           label = eq_text, 
           size = 5, 
           hjust = 0, 
           color = "black") +
  labs(
    x = "Ratio d'étudiants dans le ménage (en %)",
    y = "Transferts reçus",
    title = "Relation entre éducation et transferts reçus"
  ) +
  coord_cartesian(ylim = c(0, 10000000))



# Définition des groupes de ménages selon ratio_etude

data_menage_final <- data_menage_final %>%
  mutate(groupe_ratio = cut(ratio_etude,
                            breaks = c(0, 25, 50, 75, 100),
                            labels = c("Faible", "Modéré", "Élevé", "Très élevé"),
                            include.lowest = TRUE,
                            right = FALSE))


# Filtrer les NA avant de résumer
df_transferts_groupes <- data_menage_final %>%
  filter(!is.na(groupe_ratio), !is.na(transfert_total_menage)) %>%
  group_by(groupe_ratio) %>%
  summarise(transferts_moyens = mean(transfert_total_menage), .groups = "drop")

# Visualisations

# Diagramme en barres des moyennes par groupe
ggplot(df_transferts_groupes, aes(x = groupe_ratio, y = transferts_moyens, fill = groupe_ratio)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(transferts_moyens, 1)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Faible" = "#A6ACAF", "Modéré" = "#85929E",
                               "Élevé" = "#5D6D7E", "Très élevé" = "#34495E")) +
  labs(title = "Transferts moyens par groupe de ratio d'études",
       x = "Groupe de ratio d'études",
       y = "Montant moyen des transferts (en FCFA)") +
  theme_minimal() +
  theme(legend.position = "none")


# Boxplot par groupe
ggplot(data_menage_final, aes(x = groupe_ratio, y = transfert_total_menage, fill = groupe_ratio)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("Faible" = "#A6ACAF", "Modéré" = "#85929E",
                               "Élevé" = "#5D6D7E", "Très élevé" = "#34495E")) +
  labs(title = "Distribution des transferts par groupe de ratio d'études",
       x = "Groupe de ratio d'études",
       y = "Transferts reçus (en FCFA)") +
  theme_minimal() +
  theme(legend.position = "none")

# Relation entre transferts reçus et dépenses en santé

# Filtrer les dépenses de santé positives ou nulles
data_sante <- data_menage_final %>%
  filter(depense_sante_menage >= 0)

# Modèle de régression
modele_sante <- lm(depense_sante_menage ~ transfert_total_menage, data = data_sante)

# Extraire les paramètres
a_sante <- round(coef(modele_sante)[1], 2)
b_sante <- round(coef(modele_sante)[2], 2)
r2_sante <- round(summary(modele_sante)$r.squared, 3)
p_sante <- formatC(summary(modele_sante)$coefficients[2, 4], format = "f", digits = 4)

# Texte de l’équation
eq_text_sante <- paste0("y = ", b_sante, "x + ", a_sante,
                        "   |   R² = ", r2_sante,
                        "   |   p = ", p_sante)

# Visualisation
ggplot(data_sante, aes(x = transfert_total_menage, y = depense_sante_menage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", 
           x = 500000, 
           y = 3900000,  # Texte bien en haut
           label = eq_text_sante, 
           size = 5, 
           hjust = 0, 
           color = "black") +
  labs(
    x = "Transferts reçus par le ménage (en FCFA)",
    y = "Dépenses de santé du ménage (en FCFA)",
    title = "Relation entre transferts reçus et dépenses de santé"
  ) +
  coord_cartesian(xlim = c(0, 10000000), ylim = c(0, 4000000)) +
  theme_minimal()

# Relation entre transferts reçus et revenu total non issu du rendement agricole (emploi,non emploi et transfert)

summary_stats_filtre <- data_menage_final %>%
  filter(!is.na(part_transferts_revenu_total)) %>%
  summarise(
    Effectif = n(),
    Moyenne = mean(part_transferts_revenu_total),
    Médiane = median(part_transferts_revenu_total),
    Écart_type = sd(part_transferts_revenu_total),
    Min = min(part_transferts_revenu_total),
    Max = max(part_transferts_revenu_total),
    Q1 = quantile(part_transferts_revenu_total, 0.25),
    Q3 = quantile(part_transferts_revenu_total, 0.75)
  )

View(summary_stats_filtre)

# Diagramme en barres
data_menage_final %>%
  filter(!is.na(part_transferts_revenu_total)) %>%
  ggplot(aes(x = part_transferts_revenu_total)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", aes(y = after_stat(count / sum(count)))) +
  labs(title = "Histogramme des parts de transferts dans le revenu total",
       x = "Part des transferts dans le revenu total (%)",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()



# Histogramme avec pourcentages sans le symbole %
data_menage_final %>%
  filter(!is.na(part_transferts_revenu_total)) %>%
  ggplot(aes(x = part_transferts_revenu_total)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 5,
    fill = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  stat_bin(
    aes(y = after_stat(count / sum(count)), 
        label = round(after_stat(count / sum(count)) * 100, 1)),
    binwidth = 5,
    geom = "text",
    vjust = -0.5,
    size = 2.5
  ) +
  labs(
    title = "Histogramme des parts de transferts dans le revenu total",
    x = "Part des transferts dans le revenu total (%)",
    y = "Proportion"
  ) +
  theme_minimal()

# Corrélation entre transferts et revenus totaux

# Filtrer les données valides s'il y en a (revenu total non manquant et >= 0)
data_revenu <- data_menage_final %>%
  filter(!is.na(revenu_total), revenu_total >= 0)

# Modèle de régression
modele_revenu <- lm(revenu_total ~ transfert_total_menage, data = data_revenu)

# Extraire les paramètres
a_rev <- round(coef(modele_revenu)[1], 2)
b_rev <- round(coef(modele_revenu)[2], 2)
r2_rev <- round(summary(modele_revenu)$r.squared, 3)
p_rev <- formatC(summary(modele_revenu)$coefficients[2, 4], format = "f", digits = 4)

# Texte de l’équation
eq_text_rev <- paste0("y = ", b_rev, "x + ", a_rev,
                      "   |   R² = ", r2_rev,
                      "   |   p = ", p_rev)

# Visualisation
ggplot(data_revenu, aes(x = transfert_total_menage, y = revenu_total)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", 
           x = 500000, 
           y = max(data_revenu$revenu_total, na.rm = TRUE) * 0.95,  # Texte haut
           label = eq_text_rev, 
           size = 5, 
           hjust = 0, 
           color = "black") +
  labs(
    x = "Transferts reçus par le ménage (en FCFA)",
    y = "Revenu total du ménage (en FCFA)",
    title = "Relation entre transferts reçus et revenu total"
  ) +
  coord_cartesian(xlim = c(0, 10000000)) +
  theme_minimal()

