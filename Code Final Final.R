########## CODE FINAL ############## : ANALYSE DES DETERMINANTS DU TAUX DE FECONDITÉ EN FRANCE
### ANDREA CHAHWAN 
#### NICLETTE NDAYA NSABUA
###### LAURIANE DELPONT
########## MEHDI FEHRI

##### M1 APE 



# Chargement des librairies
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(tseries) # pour adf.test

# Charger les données

data <- read_excel("//Users/mehdifehri/Desktop/R/Données/Data R Ajustée.xlsx") %>%
  rename_with(~ gsub("-", "_", .), everything()) %>%
  # Suppression de certaines variables jugées non pertinantes
  select(-fem, -sco_jeune, -pop, -parc_logement, -viedans5, -agemat, -solo, -loyers, 
         -tailleMenage, -consoalcool, -opi_surpoids, - opi_nervosite, -chomagefem,
         -inegalité_w_privée, -tx_emploifem, -wage_h, -opi_inquietude,
         -cadre_vie,-opi_guerre, -opi_violence,-confiance_menage,-opi_affaires)

##############################################################
# VISUALISATION DES TENDANCES DES VARIABLES EXPLICATIVES
##############################################################

# Identifier les colonnes numériques (hors 'Temps' et 'fec')
numeric_cols <- setdiff(colnames(data), c("Temps", "fec"))

for (col in numeric_cols) {
  print(
    ggplot(data, aes_string(x = "Temps", y = col)) +
      geom_line(color = "blue") +
      labs(title = paste("Tendance de la variable", col), x = "Temps", y = col) +
      theme_minimal()
  )
}

##############################################################
# INTERPOLATION TRIMESTRIELLE ET DÉCALAGE TEMPOREL 
# Augmentaiton du nombres d'observations pour avoir un degré de liberté plus élevé
# Lag temporelle de 1 an car pour faire un enfant, il y'a 9 mois de gestation
##############################################################

numeric_cols <- setdiff(colnames(data), "Temps")

# Interpolation trimestrielle et création des variables décalées (lags)
data_clean <- data %>%
  complete(Temps = seq(min(Temps), max(Temps), by = 0.25)) %>%
  mutate(across(all_of(numeric_cols),
                ~ approx(Temps[!is.na(.)], .[!is.na(.)], xout = Temps)$y)) %>%
  arrange(Temps) %>%
  mutate(across(setdiff(numeric_cols, "fec"), ~ lag(., n = 4), .names = "lag_{col}")) %>%
  drop_na(starts_with("lag_"))

# Sélectionner uniquement les colonnes nécessaires pour le modèle
data_work <- data_clean %>%
  select(Temps, fec, starts_with("lag_"))

# Sauvegarder le DataFrame intermédiaire
write_xlsx(data_work, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work.xlsx")
cat("Le fichier 'Data_Work.xlsx' a été sauvegardé après l'interpolation et le lag.\n")

#préparation des variables instrumentales candidates (condition nécessaire pour les pb d'endogénéités)
Variables_instrumentales <- data_work %>% select(Temps, lag_opi_depression, lag_depseniors, lag_synthé_Oiseau, lag_lt_interest_rate, 
                                                 lag_opi_famille, lag_opi_work_fem, lag_pib_hab, lag_acceuilenf, lag_opi_niveau_vie)

# Mise à jour de `data_work` en supprimant les variables instrumentales
data_work <- data_work %>% select(-c(lag_opi_depression, lag_depseniors, lag_synthé_Oiseau, lag_lt_interest_rate, lag_opi_famille, lag_opi_work_fem,
                                     lag_pib_hab, lag_acceuilenf, lag_opi_niveau_vie))

# Sauvegarder les résultats
write_xlsx(data_work, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work.xlsx")
write_xlsx(Variables_instrumentales, "//Users/mehdifehri/Desktop/R/Code Final Fec/Variables_Instrumentales.xlsx")

library(dplyr)
rm(data)
rm(data_clean)

##############################################################
# PREPARATION DE LA SERIE CHRONOLOLGIQUE 'fec' POUR L'ANALYSE
# Transformation en série stationnaire (condition nécessaire)
##############################################################

library(readxl)
library(tseries)
library(ggplot2)
library(dplyr)

# 1. Lecture du fichier Excel
data_work <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work.xlsx")

# 2. Définir les variables explicatives : toutes sauf "fec" et "Temps"
vars_explicatives <- setdiff(colnames(data_work), c("fec", "Temps"))

# 3. Visualiser la variable fec dans le Temps
plot(data_work$Temps, data_work$fec, type = "l",
     main = "Série temporelle de fec (brute)",
     xlab = "Temps", ylab = "fec", col = "blue")

# 4. Estimation de la tendance linéaire
modele_tendance <- lm(fec ~ Temps, data = data_work)
tendance_estimee <- fitted(modele_tendance)

# Ajout de la tendance sur le graphique
lines(data_work$Temps, tendance_estimee, col = "red", lwd = 2)
title(sub = "La ligne rouge représente la tendance estimée")

# 5. Test ADF sur la série brute fec
cat("\n--- Test ADF sur la série brute (fec) ---\n")
adf_result_fec <- adf.test(data_work$fec, alternative = "stationary")
print(adf_result_fec)
if (adf_result_fec$p.value < 0.05) {
  cat("\nInterprétation : La série brute 'fec' est stationnaire (H₀ rejetée).\n")
} else {
  cat("\nInterprétation : La série brute 'fec' n'est pas stationnaire (H₀ non rejetée).\n")
}

#  Dé-trending de la série brute
data_work$fec_detrend <- data_work$fec - tendance_estimee

#  Différenciation directe de la série brute
data_work$fec_diff <- c(NA, diff(data_work$fec))

# Test ADF sur la série détrendue
cat("\n--- Test ADF sur la série détrendue (fec_detrend) ---\n")
adf_result_detrend <- adf.test(data_work$fec_detrend, alternative = "stationary")
print(adf_result_detrend)

# Test ADF sur la série brute différenciée
cat("\n--- Test ADF sur la série brute différenciée (fec_diff) ---\n")
adf_result_fec_diff <- adf.test(na.omit(data_work$fec_diff), alternative = "stationary")
print(adf_result_fec_diff)


# 9. Créer data_work_trend avec les variables nécessaires
# Inclure toutes les variables (Temps, fec, fec_detrend, fec_detrend_diff, fec_diff et création de fec_diff_relative)
data_work_trend <- data.frame(
  Temps = data_work$Temps,
  fec = data_work$fec,
  fec_detrend = data_work$fec_detrend,
  fec_diff = data_work$fec_diff
) %>%
  mutate(fec_var_relative = fec_diff / fec) # Ajouter fec_diff_relative

# Ajouter les variables explicatives
for (var in vars_explicatives) {
  data_work_trend[[var]] <- data_work[[var]]
}


# 10. Nettoyer le data_work_trend pour supprimer les lignes avec des NA
data_work_trend <- data_work_trend[complete.cases(data_work_trend), ]

# 11. Créer le dataframe fec_transformed (sans variables explicatives)
# On garde que Temps, fec, fec_detrend, fec_detrend_diff, fec_diff
fec_transformed <- data_work_trend[, c("Temps", "fec", "fec_detrend", "fec_diff","fec_var_relative")]


# 12. Visualisation séparée des séries transformées
# Fec détrendue sur le temps
plot(fec_transformed$Temps, fec_transformed$fec_detrend, type = "l", col = "purple",
     main = "Fec détrendue sur le Temps",
     xlab = "Temps", ylab = "fec_detrend")


# 13. Tableau résumé des tests ADF
# Rassembler les p-values et la conclusion stationnaire dans un tableau
stationarity_results <- data.frame(
  Variable = c("fec", "fec_detrend", "fec_diff"),
  p_value = c(adf_result_fec$p.value, adf_result_detrend$p.value, adf_result_fec_diff$p.value),
  Stationary = ifelse(c(adf_result_fec$p.value, adf_result_detrend$p.value, adf_result_fec_diff$p.value) < 0.05, 
                      "Stationary", "Non-stationary")
)

cat("\n--- Résumé des tests ADF ---\n")
print(stationarity_results)


# 1. Modèle : notre variable Fec expliquée par la tendance ? 
modele_fec_vs_detrend <- lm(fec ~ fec_detrend, data = data_work_trend)
r2_fec_vs_detrend <- summary(modele_fec_vs_detrend)$r.squared
cat(sprintf("- Fec expliquée par Fec_detrend : %.2f%%\n", r2_fec_vs_detrend * 100))

# Ajuster le dataframe data_work_trend
data_work_trend <- data_work_trend %>%
  select(Temps, fec_var_relative, everything(), -fec, -fec_detrend -fec_diff) # Réorganiser et supprimer les colonnes inutiles

write_xlsx(data_work_trend, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work_trend.xlsx")


rm(adf_result_detrend)
rm(adf_result_fec)
rm(adf_result_fec_diff)
rm(data_work)
rm(stationarity_results)
rm(modele_fec_vs_detrend)
rm(modele_tendance)


##############################################################
# Standardisation des données 
##############################################################

data_work_trend <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work_trend.xlsx")

# Renommer 'fec_diff_relative' en 'fec'
data_work_trend <- data_work_trend %>%
  rename(fec = fec_var_relative)

# 1. Modèle standardisé (centré et réduit)
data_model_standardized <- data_work_trend %>%
  mutate(across(starts_with("lag_"), ~ scale(.)))



##############################################################
# CALCUL DES R² POUR LE MODÈLE STANDARDISÉ
##############################################################

cat("\n### Calcul des R² pour le modèle standardisé ###\n")

# Calculer le \(R^2\) pour chaque variable explicative du modèle standardisé
r2_standardized <- data_model_standardized %>%
  summarise(across(starts_with("lag_"), 
                   ~ summary(lm(data_model_standardized$fec ~ .))$r.squared, 
                   .names = "R2_{col}")) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "R2") %>%
  mutate(Variable = gsub("^R2_", "", Variable)) %>%
  arrange(desc(R2))

# Afficher et sauvegarder les résultats des \(R^2\)
print(r2_standardized)
write_xlsx(r2_standardized, "//Users/mehdifehri/Desktop/R/R2_Standardized_Results.xlsx")

##############################################################
# RÉGRESSION INITIALE SUR LE MODÈLE STANDARDISÉ
##############################################################

cat("\n### Régression initiale sur le modèle standardisé ###\n")

# Modèle OLS sur le modèle standardisé
model_standardized <- lm(fec ~ . - Temps, data = data_model_standardized)
print(summary(model_standardized))


##############################################################
# DÉTECTION ET TRAITEMENT DES OUTLIERS SUR LE MODÈLE STANDARDISÉ
##############################################################

cat("\n### Détection et traitement des outliers sur le modèle standardisé ###\n")

# Étape 1 : Ajuster le modèle global sur `data_work1`
model_standardized <- lm(fec ~ ., data = data_work_trend %>% select(-Temps))

# Étape 2 : Calculer les résidus bruts et standardisés
residuals_standardized_df <- data.frame(
  Index = seq_len(nrow(data_work_trend)),
  Resid = residuals(model_standardized),      # Résidus bruts
  Std_Resid = rstandard(model_standardized),  # Résidus standardisés
  Temps = data_work_trend$Temps,
  fec = data_work_trend$fec
)

# Étape 3 : Visualiser les résidus bruts
plot_residus_bruts <- ggplot(residuals_standardized_df, aes(x = Index, y = Resid)) +
  geom_point(color = "darkorange") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(
    title = "Résidus Bruts (Modèle Standardisé)",
    x = "Index d'observation",
    y = "Résidus bruts"
  ) +
  theme_minimal()

print(plot_residus_bruts)

# Étape 4 : Visualiser les résidus standardisés (zoomé sur ±3)
threshold_zoom <- 2
plot_residus_standardises <- ggplot(residuals_standardized_df %>% filter(abs(Std_Resid) <= threshold_zoom)) +
  geom_point(aes(x = Index, y = Std_Resid), color = "blue") +
  geom_hline(yintercept = c(-1, 1), color = "red", linetype = "dashed") +
  labs(
    title = "Résidus Standardisés (Zoomé)",
    x = "Index d'observation",
    y = "Résidus standardisés"
  ) +
  theme_minimal()

print(plot_residus_standardises)

# Étape 5 : Identifier les outliers
threshold <-2   # Seuil pour identifier les outliers
outliers_standardized_df <- residuals_standardized_df %>%
  filter(abs(Std_Resid) > threshold)

cat("\n### Résumé des outliers identifiés ###\n")
print(outliers_standardized_df)

# Étape 6 : Créer un nouveau DataFrame sans les outliers (`data_work2`)
data_work2 <- data_work_trend %>%
  mutate(Index = seq_len(nrow(data_work_trend))) %>%
  filter(!Index %in% outliers_standardized_df$Index) %>%
  select(-Index)

# Étape 7 : Tableau récapitulatif des outliers
recap_outliers <- outliers_standardized_df %>%
  select(Temps, fec, Std_Resid)

# Sauvegarder les résultats finaux
write_xlsx(data_work2, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work2.xlsx")
write_xlsx(recap_outliers, "//Users/mehdifehri/Desktop/R/Code Final Fec/Recap_Outliers.xlsx")

rm(recap_outliers)
rm(model_standardized)
rm(outliers_standardized_df)
rm(residuals_standardized_df)

##############################################################
# PARTIE 2 : DÉTECTION DES ALIAS (COLINÉARITÉ PARFAITE)
##############################################################

cat("\n### Détection des alias (colinéarité parfaite) ###\n")

# Étape 1 : Ajuster le modèle global pour `data_work2`
model_alias <- lm(fec ~ ., data = data_work2 %>% select(-Temps))

# Étape 2 : Utiliser le modèle global pour identifier les alias
alias_info <- alias(model_alias)

# Étape 3 : Extraire les colinéarités parfaites
alias_matrix <- alias_info$Complete  # Matrice de colinéarités parfaites

# Vérifier si des alias existent
if (is.null(alias_matrix)) {
  cat("Aucune colinéarité parfaite détectée dans le modèle.\n")
} else {
  # Étape 4 : Identifier les variables colinéaires
  alias_pairs <- which(alias_matrix != 0, arr.ind = TRUE)
  
  # Extraire les noms des variables impliquées dans les colinéarités
  alias_summary <- data.frame(
    Variable_1 = rownames(alias_matrix)[alias_pairs[, 1]],
    Variable_2 = colnames(alias_matrix)[alias_pairs[, 2]]
  ) %>%
    distinct()
  
  # Afficher la liste des colinéarités parfaites
  cat("Colinéarités parfaites détectées :\n")
  print(alias_summary)
  
# Étape 5 : Sauvegarder les colinéarités dans un fichier Excel
write_xlsx(alias_summary, "//Users/mehdifehri/Desktop/R/Colinear_Variables_Data_Work2.xlsx")
  cat("La liste des colinéarités parfaites a été sauvegardée dans 'Colinear_Variables_Data_Work2.xlsx'.\n")
}

# Étape 6 : Extraire les variables concernées par les alias
alias_vars <- rownames(alias_info$Complete)
cat("Variables impliquées dans les alias avec l'intercept :\n")
print(alias_vars)

# Étape 7 : Supprimer les variables alias et créer un nouveau DataFrame `data_work3`
data_work3 <- data_work2 %>%
  select(-all_of(alias_vars))

# Étape 8 : Sauvegarder le DataFrame mis à jour
write_xlsx(data_work3, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work3.xlsx")
cat("Le nouveau DataFrame sans alias a été sauvegardé sous 'Data_Work3.xlsx'.\n")

# Étape 9 : Résumer les variables supprimées
removed_variables_summary <- data.frame(Variables_Supprimées = alias_vars)
write_xlsx(removed_variables_summary, "//Users/mehdifehri/Desktop/R/Code Final Fec/Removed_Variables_Alias.xlsx")
cat("Résumé des variables supprimées enregistré sous 'Removed_Variables_Alias.xlsx'.\n")

rm(data_work2)
rm(alias_info)
rm(model_alias)
rm(removed_variables_summary)
rm(alias_matrix)
rm(alias_pairs)
rm(alias_summary)

##############################################################
# PARTIE 3 : ANALYSE DES VARIABLES EXPLICATIVES (CORRÉLATION, MDS, etc.)
##############################################################

library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(reshape2)
library(stats)
library(igraph)

# Charger le DataFrame `data_work3` directement
data_expl <- data_work3 %>%
  select(-Temps, -fec)  # Exclure les colonnes non explicatives

# Calculer la matrice de corrélation
cor_mat <- cor(data_expl, use = "complete.obs")
write_xlsx(as.data.frame(cor_mat), "//Users/mehdifehri/Desktop/R/Code Final Fec/Correlation_Matrix_Data_Work3.xlsx")

cat("Matrice de corrélation (sans variable dépendante) :\n")
print(cor_mat)

# Heatmap
correlation_melted <- melt(cor_mat)
heatmap_plot <- ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Matrice de corrélation (data_work3)", x = "Variables", y = "Variables")

print(heatmap_plot)
ggsave("//Users/mehdifehri/Desktop/R/Code Final Fec/Correlation_Heatmap_Data_Work3.png", plot = heatmap_plot,
       width = 10, height = 8, dpi = 300)

cat("Matrice de corrélation affichée, sauvegardée en Excel et heatmap PNG.\n")

# Identification des paires de variables fortement corrélées
corr_threshold <- 0.5
high_corr_pairs <- correlation_melted %>%
  filter(value > corr_threshold & Var1 != Var2) %>%
  arrange(desc(value)) %>%
  mutate(pair = paste0(pmin(as.character(Var1), as.character(Var2)), "-", pmax(as.character(Var1), as.character(Var2)))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

cat("Paires de variables avec corrélation >", corr_threshold, ":\n")
print(high_corr_pairs)
write_xlsx(high_corr_pairs, "//Users/mehdifehri/Desktop/R/Code Final Fec/High_Correlation_Pairs_Data_Work3.xlsx")

# Compter les variables les plus fréquentes dans les paires corrélées
variable_counts <- high_corr_pairs %>%
  select(Var1, Var2) %>%
  pivot_longer(cols = everything(), values_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

cat("\n### Variables les plus corrélées ###\n")
print(variable_counts)


##############################################################
# 4. VISUALISATION DE LA STRUCTURE DES VARIABLES (DENDRO, MDS, RÉSEAU)
##############################################################

cat("\n### Visualisation des structures des variables ###\n")

# Création d'une matrice de distance basée sur 1 - |corr|
dist_mat <- as.dist(1 - abs(cor_mat))


### MDS (Multi-Dimensional Scaling)
mds_res <- cmdscale(dist_mat, k = 2)
mds_df <- data.frame(
  Dim1 = mds_res[,1],
  Dim2 = mds_res[,2],
  Variable = rownames(mds_res)
)

mds_plot <- ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Variable)) +
  geom_point() +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Représentation MDS des variables (data_work3)", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()

print(mds_plot)
ggsave("//Users/mehdifehri/Desktop/R/Code Final Fec/MDS_Plot_Data_Work3.pdf", plot = mds_plot, width = 10, height = 8)
cat("MDS plot affiché et sauvegardé en PDF.\n")

### Graphe de réseau
high_corr <- melt(cor_mat) %>%
  filter(value > corr_threshold & Var1 != Var2) %>%
  mutate(pair = paste(pmin(as.character(Var1), as.character(Var2)),
                      pmax(as.character(Var1), as.character(Var2)), sep = "-")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

g <- graph_from_data_frame(high_corr[, c("Var1", "Var2")], directed = FALSE)

plot(g,
     layout = layout_with_fr(g),
     vertex.size = 5,
     vertex.label.cex = 0.5,
     main = paste("Graphe de réseau des variables (corr >", corr_threshold, ")"))
cat("Graphe de réseau affiché.\n")

rm(g)
rm(hc)
rm(high_corr)
rm(high_corr_pairs)
rm(mds_df)
rm(mds_res)
rm(variable_counts)
rm(data_expl)
rm(cor_mat)
rm(correlation_melted)
rm(data_work_trend)
rm(fec_transformed)
rm(alias_summary)

#####################################################################
# Test VIF et suppression des variables avec VIF élevé (standardisation incluse)
#####################################################################

# Charger les bibliothèques nécessaires
library(car)  # Pour le calcul du VIF
library(dplyr)
library(writexl)

data_work3 <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work3.xlsx")

cat("\n### Début de la détection des VIF élevés dans data_work3 ###\n")

# Identifier les variables explicatives
variables_explicatives <- setdiff(names(data_work3), c("fec", "Temps"))

# Créer une copie des données pour le processus VIF
data_for_vif <- data_work3

# Définir le seuil de VIF
vif_threshold <- 666

# Initialiser les listes pour stocker les résultats
iteration_results <- list()
removed_variables <- data.frame(Iteration = integer(), Variable = character(), VIF_Value = numeric(), stringsAsFactors = FALSE)

# Boucle itérative pour supprimer les variables avec VIF élevé
iteration <- 1
while (TRUE) {
  cat("\n--- Itération", iteration, "---\n")
  
  # Ajuster un modèle linéaire avec les variables explicatives restantes
  current_model <- lm(fec ~ ., data = data_for_vif[, c("fec", variables_explicatives)])
  
  # Calculer le VIF pour chaque variable explicative
  vif_values <- vif(current_model)
  
  # Afficher les VIF actuels
  cat("Facteurs d'inflation de la variance (VIF) actuels :\n")
  print(vif_values)
  
  # Sauvegarder les résultats de l'itération
  iteration_results[[iteration]] <- data.frame(Variable = names(vif_values), VIF = vif_values, Iteration = iteration)
  
  # Identifier les variables avec un VIF supérieur au seuil
  high_vif_vars <- names(vif_values[vif_values > vif_threshold])
  
  # Vérifier s'il reste des variables avec un VIF élevé
  if (length(high_vif_vars) == 0) {
    cat("Toutes les variables ont un VIF <= ", vif_threshold, ". Fin de la boucle.\n")
    break
  }
  
  # Identifier la variable avec le VIF maximum
  variable_to_remove <- high_vif_vars[which.max(vif_values[high_vif_vars])]
  max_vif_value <- max(vif_values[high_vif_vars])
  cat("Variable avec le VIF le plus élevé :", variable_to_remove, "(", max_vif_value, ")\n")
  
  # Ajouter la variable supprimée à la liste des variables supprimées
  removed_variables <- rbind(removed_variables, data.frame(Iteration = iteration, Variable = variable_to_remove, VIF_Value = max_vif_value))
  
  # Supprimer cette variable des données et des variables explicatives
  data_for_vif <- data_for_vif %>% select(-all_of(variable_to_remove))
  variables_explicatives <- setdiff(variables_explicatives, variable_to_remove)
  
  # Augmenter le compteur d'itérations
  iteration <- iteration + 1
}

data_work4 <- data_for_vif

# Sauvegarder les données finales sans VIF élevé
write_xlsx(data_work4, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4.xlsx")
cat("Les données finales après suppression des variables avec VIF > ", vif_threshold, " ont été sauvegardées sous 'Data_Work4.xlsx'.\n")


# Imprimer les noms des colonnes du nouveau DataFrame `data_work4`
cat("\nNoms des colonnes du DataFrame `data_work4` :\n")
print(names(data_for_vif))

# Sauvegarder le DataFrame
write_xlsx(data_work4, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_work4.xlsx")
cat("Le fichier a été sauvegardé avec succès.\n")



rm(current_model)
rm(data_work3)
rm(iteration_results)
rm(removed_variables)
rm(data_for_vif)


##############################################################
####### Régression sur modèle initial et modèle auxiliaire log #######
##############################################################

library(readxl)
library(dplyr)
library(lmtest)
library(writexl)

# Charger les données
data_work4 <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4.xlsx")

# Modèle OLS de base
model_ols <- lm(fec ~ . - Temps, data = data_work4)
print(summary(model_ols))

# Création d'un modèle auxiliaire avec variables explicatives log
# Transformer les variables explicatives en log avec un préfixe et nettoyer les non-log
data_work4_log <- data_work4 %>%
  mutate(across(-c(Temps, fec), log, .names = "log_{.col}")) %>%
  select(Temps, fec, starts_with("log_"))
write_xlsx(data_work4_log, "//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4_Log.xlsx")

# Modèle OLS avec les variables explicatives transformées en log
model_ols_log <- lm(fec ~ . - Temps, data = data_work4_log)
print(summary(model_ols_log))

# Test RESET pour le modèle OLS (base)
cat("\n### Test RESET pour le modèle OLS ###\n")
reset_test_ols <- resettest(model_ols, power = 2:3, type = "fitted")
print(reset_test_ols)

if (reset_test_ols$p.value > 0.05) {
  cat("\nInterprétation : Le modèle de base est correctement spécifié. (p-value =", reset_test_ols$p.value, ")\n")
} else {
  cat("\nInterprétation : Le modèle de base est mal spécifié. (p-value =", reset_test_ols$p.value, ")\n")
}

# Test RESET pour le modèle OLS log-transformé
cat("\n### Test RESET pour le modèle OLS (log-transformé) ###\n")
reset_test_ols_log <- resettest(model_ols_log, power = 2:3, type = "fitted")
print(reset_test_ols_log)

if (reset_test_ols_log$p.value > 0.05) {
  cat("\nInterprétation : Le modèle log-transformé est correctement spécifié. (p-value =", reset_test_ols_log$p.value, ")\n")
} else {
  cat("\nInterprétation : Le modèle log-transformé est mal spécifié. (p-value =", reset_test_ols_log$p.value, ")\n")
}

##############################################################
# TABLEAU COMPARATIF DES RÉSULTATS
##############################################################

# Initialisation du tableau récapitulatif
model_comparison <- data.frame(
  Model = character(),
  Adjusted_R2 = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  RESET_p_value = numeric(),
  RESET_Interpretation = character(),
  stringsAsFactors = FALSE
)

# Remplir le tableau pour chaque modèle
models <- list(
  "Model OLS" = model_ols,
  "Model OLS (Log)" = model_ols_log
)

# Fonction pour obtenir l'interprétation du test RESET
perform_reset <- function(model) {
  reset_results <- resettest(model, power = 2:3, type = "fitted")
  if (reset_results$p.value > 0.05) {
    interpretation <- "Correctement spécifié"
  } else {
    interpretation <- "Mal spécifié"
  }
  return(list(p_value = reset_results$p.value, interpretation = interpretation))
}

# Remplir le tableau avec les résultats pour chaque modèle
for (name in names(models)) {
  model <- models[[name]]
  summary_model <- summary(model)
  reset_results <- perform_reset(model)
  
  model_comparison <- rbind(model_comparison, data.frame(
    Model = name,
    Adjusted_R2 = summary_model$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model),
    RESET_p_value = reset_results$p_value,
    RESET_Interpretation = reset_results$interpretation
  ))
}

# Afficher le tableau comparatif des modèles
print(model_comparison)

# Sauvegarder le tableau comparatif dans un fichier Excel
write_xlsx(model_comparison, "//Users/mehdifehri/Desktop/R/Code Final Fec/Model_Comparison.xlsx")

rm(model)
rm(model_comparison)
rm(models)
rm(summary_model)
rm(model_ols)
rm(model_ols_log)
rm(reset_results)
rm(reset_test_ols)
rm(reset_test_ols_log)

# Charger les bibliothèques nécessaires
library(broom)
library(writexl)
library(dplyr)
library(readxl)

#############################################################
# Étape 1 : Détection et suppréssion des outliers basés sur le nouveau model réduit double-log
### Réponse et Solution à un problème d'Hétéroscédasticité BP, et White identifié avant
#############################################################

# Chargement des données
data_work4_log <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4_log.xlsx")

# Définition des variables explicatives (exclusion de "Temps" et "fec")
variables_explicatives_ols <- setdiff(colnames(data_work4_log), c("Temps", "fec"))

# Ajustement du modèle initial
model_final <- lm(as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + "))), data = data_work4_log)

# Extraction des résidus
residus <- residuals(model_final)

# Seuil pour les outliers (par exemple, 2 fois l'écart-type des résidus)
threshold <- 2 * sd(residus)

# Suppression des outliers
data_work4_log_out <- data_work4_log %>%
  mutate(Residus = residus) %>%
  filter(abs(Residus) <= threshold) %>%
  select(-Residus)  # Retirer la colonne temporaire "Residus"

# Régression OLS après suppression des outliers
model_after_outliers <- lm(as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + "))), 
                           data = data_work4_log_out)

# Résumé du modèle ajusté après suppression des outliers
cat("\nRésumé du modèle OLS après suppression des outliers:\n")
print(summary(model_after_outliers))
write_xlsx(data_work4_log_out, "//Users/mehdifehri/Desktop/R/Code Final Fec/data_work4_log_out.xlsx")


rm(data_model_standardized)
rm(data_work4)
rm(data_work4_log)

#############################################################
# Modèle final et visualisation des résidus
#############################################################

data_work4_log_out <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/Data_Work4_log_out.xlsx")
variables_explicatives_ols <- setdiff(colnames(data_work4_log_out), c("Temps", "fec"))
formule_ols <- as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + ")))

# Ajustement du modèle de régression
model_final <- lm(formule_ols, data = data_work4_log_out)

# Extraction des résidus
residus <- residuals(model_final)

cat("\n### Visualisations des résidus ###\n")

# Résidus vs valeurs ajustées
plot(fitted(model_final), residus, main = "Résidus vs valeurs ajustées",
     xlab = "Valeurs ajustées", ylab = "Résidus", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Résidus vs indices
plot(1:length(residus), residus, main = "Résidus vs indices",
     xlab = "Indice", ylab = "Résidus", pch = 19, col = "green")
abline(h = 0, col = "red", lty = 2)

# Histogramme des résidus
hist(residus, breaks = 15, col = "gray", main = "Histogramme des résidus", xlab = "Résidus")

# QQ-Plot des résidus
qqnorm(residus, main = "Q-Q Plot des résidus")
qqline(residus, col = "red")

#############################################################
# Hypothèses Normalité  
#############################################################

# Test de Shapiro-Wilk
shapiro_test <- shapiro.test(residus)
cat("Test de Shapiro-Wilk :\n")
cat("Statistique W :", round(shapiro_test$statistic, 4), "\n")
cat("P-value :", round(shapiro_test$p.value, 4), "\n")

if (shapiro_test$p.value > 0.05) {
  cat("Conclusion : Les résidus suivent une distribution normale (H0 acceptée).\n")
} else {
  cat("Conclusion : Les résidus ne suivent pas une distribution normale (H0 rejetée).\n")
}

####################################################
# HYPOTHÈSE 1 : Résidus de moyenne nulle
####################################################
cat("\n--- Étape 1 : Résidus de moyenne nulle ---\n")
mean_residuals <- mean(residus)
std_error_residuals <- sd(residus) / sqrt(length(residus))
t_stat <- mean_residuals / std_error_residuals
p_value <- 2 * pt(-abs(t_stat), df = length(residus) - 1)

cat("Moyenne des résidus :", round(mean_residuals, 5), "\n")
cat("t-statistic :", round(t_stat, 3), " | p-value :", round(p_value, 5), "\n")

if (p_value > 0.05) {
  cat("H₀ est vérifiée : les résidus ont une moyenne nulle.\n")
} else {
  cat("H₀ est rejetée : les résidus n'ont pas une moyenne nulle.\n")
}

###########################################################
#####  HYPOTHÈSE 2 - EXOGÉNÉITÉ (CONDITION NECESSAIRE)
#########################################################

library(readxl)
library(dplyr)
library(lmtest)
library(ggplot2)
library(systemfit)
library(tseries)

# Chargement des données
data_work4_log_out <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/data_work4_log_out.xlsx")

# Définition des variables explicatives
variables_explicatives_ols <- setdiff(colnames(data_work4_log_out), c("Temps", "fec"))
formule_ols <- as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + ")))

# Ajustement du modèle
model_final <- lm(formule_ols, data = data_work4_log_out)

# Extraction des résidus et des valeurs ajustées
residus <- residuals(model_final)
valeurs_ajustees <- fitted(model_final)

#############################################################
# Étape 1 : Visualisation des résidus
#############################################################

cat("\n### Visualisation des résidus ###\n")

# Graphique des résidus vs chaque variable explicative
for (var in variables_explicatives_ols) {
  # Création explicite du dataframe
  data_plot <- data.frame(
    x = data_work4_log_out[[var]],
    residus = residus
  )
  
  # Création du graphique
  plot <- ggplot(data_plot, aes(x = x, y = residus)) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    theme_minimal() +
    labs(
      title = paste("Résidus vs", var),
      x = var,
      y = "Résidus"
    )
  
  # Forcer l'affichage du graphique dans la boucle
  print(plot)
}

# Corrélation entre résidus et variables explicatives
#############################################################

cat("\n### Corrélation entre résidus et variables explicatives ###\n")

# Calculer les corrélations entre résidus et les variables explicatives
correlations <- sapply(variables_explicatives_ols, function(var) {
  cor.test(residus, data_work4_log_out[[var]])$estimate
})
p_values <- sapply(variables_explicatives_ols, function(var) {
  cor.test(residus, data_work4_log_out[[var]])$p.value
})

# Résultats sous forme de tableau
corr_results <- data.frame(
  Variable = variables_explicatives_ols,
  Correlation = correlations,
  P_Value = p_values,
  Significant = p_values <= 0.05
)
print(corr_results)

if (any(corr_results$Significant)) {
  cat("\nCertaines variables explicatives sont significativement corrélées aux résidus. Risque d'endogénéité détecté.\n")
} else {
  cat("\nAucune variable explicative n'est significativement corrélée aux résidus. Pas d'évidence d'endogénéité.\n")
}

# Étape 3 : Causalité inversée ? (Test de Granger)
#############################################################

cat("\n### Test de causalité de Granger ###\n")

# Tester la causalité de Granger pour chaque variable explicative

# Tester la causalité de Granger pour chaque variable explicative et chaque ordre
orders <- 1:8  # Ordres des tests
granger_results <- lapply(variables_explicatives_ols, function(var) {
  lapply(orders, function(order) {
    tryCatch({
      test <- grangertest(data_work4_log_out[[var]] ~ residus, order = order, data = data_work4_log_out)
      data.frame(
        Variable = var,
        Order = order,
        P_Value = test$`Pr(>F)`[2],
        Significant = test$`Pr(>F)`[2] <= 0.05
      )
    }, error = function(e) {
      data.frame(
        Variable = var,
        Order = order,
        P_Value = NA,
        Significant = NA
      )
    })
  }) %>% bind_rows()
}) %>% bind_rows()

# Afficher les résultats pour tous les tests
print(granger_results)

# Identifier les variables potentiellement endogènes
potentially_endogenous <- granger_results %>%
  filter(Significant == TRUE) %>%
  distinct(Variable)  # Supprimer les doublons

# Générer un tableau des variables potentiellement endogènes
cat("\nVariables potentiellement endogènes (significatives au moins une fois) :\n")
print(potentially_endogenous)

# Étape 4 : Analyse des résidus croisés
#############################################################

cat("\n### Analyse des résidus croisés ###\n")

# Régression des variables explicatives sur les résidus
cross_results <- lapply(variables_explicatives_ols, function(var) {
  model_residu <- lm(data_work4_log_out[[var]] ~ residus)
  summary_model <- summary(model_residu)
  data.frame(
    Variable = var,
    Coefficient = coef(summary_model)["residus", "Estimate"],
    P_Value = coef(summary_model)["residus", "Pr(>|t|)"],
    Significant = coef(summary_model)["residus", "Pr(>|t|)"] <= 0.05
  )
}) %>% bind_rows()

print(cross_results)

rm(corr_results)
rm(cross_results)
rm(granger_results)

##############################################
########## Variables instrumentales
#############################################
# Charger les bibliothèques nécessaires
library(readxl)
library(dplyr)
library(writexl)
library(AER)
library(lmtest)
library(sandwich)
library(car)

########################################
### 1. Chargement des données
########################################
Variables_instrumentales1 <- read_excel("//Users/mehdifehri/Desktop/R/Code Final Fec/Variables_Instrumentales.xlsx")
data_work4_log_out <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/data_work4_log_out.xlsx")

cat("Variables disponibles dans data_work4_log_out:\n")
print(colnames(data_work4_log_out))

########################################
### 2. Préparation des données
########################################
# Transformation des variables instrumentales (log si > 0)
Vi_vf <- Variables_instrumentales1 %>%
  filter(Temps %in% data_work4_log_out$Temps) %>%
  mutate(across(-Temps, ~ ifelse(. > 0, log(.), NA), .names = "log_{.col}")) %>%
  select(starts_with("log_")) %>%
  select(-log_lag_opi_depression, -log_lag_opi_niveau_vie)  # Suppression de la variable log_lag_depression

# Définir variables explicatives et endogènes
variables_explicatives <- setdiff(colnames(data_work4_log_out), c("Temps", "fec"))
endogenous_vars <- variables_explicatives[grep("(env|tpspartiel|cadrefemme|nuptialite|affaires|preca)", variables_explicatives)]

cat("\nVariables endogènes identifiées:\n")
print(endogenous_vars)

# Variables instrumentales
instrument_vars <- colnames(Vi_vf)
cat("\nVariables instrumentales disponibles:\n")
print(instrument_vars)

# Combiner les données
data_work4_log_out_iv <- bind_cols(data_work4_log_out, Vi_vf)


#######################################
### 4. Tests de première étape (First-stage) pour les variables endogènes 2FSLS
########################################

first_stage_summary <- data.frame()

if (length(endogenous_vars) > 0) {
  cat("\n=== Tests de première étape (First-Stage Regressions) ===\n")
  
  for (endog in endogenous_vars) {
    cat("\n--- Analyse de la variable endogène :", endog, "---\n")
    
    # Formule pour la première étape
    first_stage_formula <- as.formula(paste(
      endog, "~",
      paste(c(instrument_vars, setdiff(variables_explicatives, endogenous_vars)), collapse = " + ")
    ))
    
    # Ajustement du modèle
    first_stage_model <- lm(first_stage_formula, data = data_work4_log_out_iv)
    
    # Résumé du modèle
    cat("\nRésumé du modèle de première étape pour", endog, ":\n")
    print(summary(first_stage_model))
    
    # F-test pour vérifier la pertinence des instruments
    cat("\n--- Test de pertinence des instruments pour", endog, "---\n")
    instruments_only_formula <- as.formula(paste(endog, "~", paste(instrument_vars, collapse = " + ")))
    instruments_only <- update(first_stage_model, instruments_only_formula)
    f_test <- waldtest(first_stage_model, instruments_only)
    
    cat("\nRésultats du F-test :\n")
    print(f_test)
    
    # Stockage des résultats pour résumé
    f_stat <- ifelse(!is.null(f_test$F[2]), f_test$F[2], NA)
    f_p_value <- ifelse(!is.null(f_test$`Pr(>F)`[2]), f_test$`Pr(>F)`[2], NA)
    r_squared <- summary(first_stage_model)$r.squared
    adj_r_squared <- summary(first_stage_model)$adj.r.squared
    
    first_stage_summary <- rbind(first_stage_summary,
                                 data.frame(
                                   Endogenous_Variable = endog,
                                   F_statistic = f_stat,
                                   F_p_value = f_p_value,
                                   R_squared = r_squared,
                                   Adj_R_squared = adj_r_squared
                                 ))
    
    # Résumé des résultats pour cette variable endogène
    cat("\n--- Résultats pour", endog, "---\n")
    cat("F-statistic :", round(f_stat, 4), "\n")
    cat("P-value (F-test) :", round(f_p_value, 4), "\n")
    cat("R-squared :", round(r_squared, 4), "\n")
    cat("Adjusted R-squared :", round(adj_r_squared, 4), "\n")
    cat("\n--------------------------\n")
  }
  
  # Résumé final
  cat("\n=== Résultats agrégés des premières étapes ===\n")
  print(first_stage_summary)
  
  # Interprétation
  cat("\nInterprétation du test de première étape :\n")
  cat("- Un F-statistic élevé (souvent > 10) indique que les instruments sont pertinents.\n")
  cat("- Si le F-test est faible, cela suggère que les instruments sont faibles et le modèle IV risque d'être biaisé.\n")
  
} else {
  cat("\nAucune variable endogène trouvée. Veuillez vérifier les noms des variables.\n")
  cat("Interprétation : Sans variable endogène, pas besoin de passer par un modèle IV.\n")
}

########################################
### 5. Estimation du modèle IV, Test de Sargan et Test de Hausman
########################################

# Formule IV
formula_iv <- as.formula(paste(
  "fec ~", 
  paste(variables_explicatives, collapse = " + "),
  "|",
  paste(c(setdiff(variables_explicatives, endogenous_vars), instrument_vars), collapse = " + ")
))

# Estimation du modèle IV
iv_model <- ivreg(formula_iv, data = data_work4_log_out_iv)

cat("\nRésumé du modèle IV:\n")
print(summary(iv_model))

### test de Sargan 

# Vérifiez que le modèle IV a été estimé avant de lancer ce test
if (!exists("iv_model")) {
  stop("Le modèle IV n'a pas encore été estimé. Veuillez exécuter le bloc pour l'estimation du modèle IV.")
}

# Test de Sargan
iv_summary <- summary(iv_model, diagnostics = TRUE)
sargan_test <- iv_summary$diagnostics["Sargan", ]

cat("\nTest de Sargan:\n")
print(sargan_test)

cat("\nInterprétation du Test de Sargan:\n")
cat("- Un p-value faible (<0.05) indique que les instruments ne sont pas valides (suridentification non rejetée).\n")
cat("- Un p-value élevé indique que les instruments sont globalement valides.\n")

# comparaison OLS 

ols_model <- lm(as.formula(paste("fec ~", paste(variables_explicatives, collapse = " + "))),
                data = data_work4_log_out_iv)

cat("\nRésumé du modèle OLS (pour comparaison):\n")
print(summary(ols_model))


# Vérifiez que les modèles IV et OLS ont été estimés avant de lancer ce test
if (!exists("iv_model") || !exists("ols_model")) {
  stop("Les modèles IV et OLS doivent être estimés avant d'exécuter le test de Hausman.")
}

# Test de Hausman
hausman_results <- try({
  b_iv <- coef(iv_model)
  b_ols <- coef(ols_model)
  common_vars <- intersect(names(b_iv), names(b_ols))
  
  b_diff <- b_iv[common_vars] - b_ols[common_vars]
  vcov_diff <- vcov(iv_model)[common_vars, common_vars] - vcov(ols_model)[common_vars, common_vars]
  
  if (det(vcov_diff) == 0) {
    stop("La matrice de variance-covariance est singulière, impossible de calculer le test de Hausman.")
  }
  
  stat <- as.numeric(t(b_diff) %*% solve(vcov_diff) %*% b_diff)
  p_value <- 1 - pchisq(stat, df = length(common_vars))
  list(statistic = stat, p_value = p_value)
})

if (!inherits(hausman_results, "try-error")) {
  cat("\nTest de Hausman:\n")
  print(hausman_results)
  cat("\nInterprétation du Test de Hausman:\n")
  cat("- Un p-value faible suggère que le modèle OLS est biaisé et que le modèle IV est préférable.\n")
  cat("- Un p-value élevé suggère que le biais d'endogénéité n'est pas significatif, l'OLS pourrait être acceptable.\n")
} else {
  cat("Le test de Hausman n'a pas pu être effectué (matrice non inversible). Vérifiez vos données et instruments.\n")
}

########################################
### 6. Création du tableau récapitulatif des résultats
########################################

# Construire le tableau récapitulatif
summary_table <- data.frame(
  Test = c(
    "First-Stage Regression (log_lag_confiance_menage)",
    "First-Stage Regression (log_lag_opi_niveau_vie)",
    "Sargan Test",
    "Hausman Test"
  ),
  Statistic = c(
    "F-statistic",
    "F-statistic",
    "Chi-Squared",
    "Chi-Squared"
  ),
  Value = c(
    first_stage_summary$F_statistic[1],
    first_stage_summary$F_statistic[2],
    sargan_test["statistic"],
    ifelse(!inherits(hausman_results, "try-error"), hausman_results$statistic, NA)
  ),
  P_Value = c(
    first_stage_summary$F_p_value[1],
    first_stage_summary$F_p_value[2],
    sargan_test["p-value"],
    ifelse(!inherits(hausman_results, "try-error"), hausman_results$p_value, NA)
  ),
  Conclusion = c(
    ifelse(first_stage_summary$F_statistic[1] > 10, 
           "Instruments are strong and relevant for log_lag_confiance_menage.",
           "Instruments might be weak for log_lag_confiance_menage."),
    ifelse(first_stage_summary$F_statistic[2] > 10, 
           "Instruments are strong and relevant for log_lag_opi_niveau_vie.",
           "Instruments might be weak for log_lag_opi_niveau_vie."),
    ifelse(sargan_test["p-value"] > 0.05, 
           "Instruments are globally valid.", 
           "Instruments may not be valid."),
    ifelse(!inherits(hausman_results, "try-error") && hausman_results$p_value > 0.05, 
           "OLS is acceptable as no significant endogeneity bias is detected.",
           "IV model is preferable due to significant endogeneity.")
  )
)

# Afficher le tableau récapitulatif dans la console
cat("\n=== Tableau récapitulatif des résultats ===\n")
print(summary_table)

# Optionnel : Sauvegarder le tableau dans un fichier Excel
write_xlsx(summary_table, "//Users/mehdifehri/Desktop/R/Code Final Fec/Test_Summary.xlsx")
cat("\nLe tableau récapitulatif des résultats a été sauvegardé dans 'Test_Summary.xlsx'.\n")

rm(data_plot)
rm(f_test)
rm(first_stage_summary)
rm(first_stage_model)
rm(hausman_results)
rm(instruments_only)
rm(iv_model)
rm(iv_summary)
rm(plot_residus_bruts)
rm(plot_residus_standardises)
rm(r2_standardized)
rm(Variables_instrumentales)
rm(Variables_instrumentales1)

###########################################################
#####      HYPOTHÈSE 3 - Hétéroscédasticité
#########################################################

# Chargement des données et ajustement du modèle

library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(lmtest)

# Chargement des données
data_work4_log_out <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/data_work4_log_out.xlsx")

variables_explicatives_ols <- setdiff(colnames(data_work4_log_out), c("Temps", "fec"))
formule_ols <- as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + ")))

# Ajustement du modèle
model_final <- lm(formule_ols, data = data_work4_log_out)

# Extraction des résidus et des valeurs ajustées
residus <- residuals(model_final)
valeurs_ajustees <- fitted(model_final)

# 1. Visualisation de l’hétéroscédasticité

cat("\n--- Visualisation de l’hétéroscédasticité ---\n")

# Résidus vs Valeurs ajustées
ggplot(data.frame(valeurs_ajustees, residus), aes(x = valeurs_ajustees, y = residus)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Graphique des résidus vs valeurs ajustées",
    x = "Valeurs ajustées",
    y = "Résidus"
  )

# Résidus vs variables explicatives
for (var in variables_explicatives_ols) {
  # Construire le dataframe avec des noms explicites
  data_plot <- data.frame(
    residus = residus,
    x = data_work4_log_out[[var]]
  )
  
  # Créer le graphique
  plot <- ggplot(data_plot, aes(x = x, y = residus)) +
    geom_point(color = "green", alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(
      title = paste("Graphique des résidus vs", var),
      x = var,
      y = "Résidus"
    )
  
  # Forcer l'affichage avec print()
  print(plot)
}


# 2. Tests d’hétéroscédasticité : Breusch-Pagan
#############################################################

cat("\n--- Test de Breusch-Pagan ---\n")

# Détail de la régression auxiliaire
bp_model <- lm(residus^2 ~ ., data = data_work4_log_out[, variables_explicatives_ols])
summary_bp <- summary(bp_model)

cat("\nRésumé de la régression auxiliaire :\n")
print(summary_bp)

# Test de Breusch-Pagan
test_bp <- bptest(model_final)

cat("Statistique du test :", round(test_bp$statistic, 3), "\n")
cat("P-value :", round(test_bp$p.value, 5), "\n")

if (test_bp$p.value > 0.05) {
  cat("H₀ est vérifiée : pas d'évidence d'hétéroscédasticité (Breusch-Pagan).\n")
} else {
  cat("H₀ est rejetée : présence d'hétéroscédasticité (Breusch-Pagan).\n")
}


# Test de White avec détail
#############################################################

cat("\n--- Test de White ---\n")

# Régression auxiliaire pour le test de White
white_model <- lm(residus^2 ~ poly(valeurs_ajustees, 2), data = data.frame(residus, fitted(model_final)))
summary_white <- summary(white_model)

cat("\nRésumé de la régression auxiliaire (White) :\n")
print(summary_white)

# Statistique de White
white_stat <- summary_white$r.squared * nrow(data_work4_log_out)  # Statistique chi-carré basée sur le R²
white_pval <- pchisq(white_stat, df = 2, lower.tail = FALSE)  # Degré de liberté = 2 (termes quadratiques)

cat("Statistique du test :", round(white_stat, 3), "\n")
cat("P-value :", round(white_pval, 5), "\n")

if (white_pval > 0.05) {
  cat("H₀ est vérifiée : pas d'évidence d'hétéroscédasticité (White).\n")
} else {
  cat("H₀ est rejetée : présence d'hétéroscédasticité (White).\n")
}

#############################################################
# Résumé des tests
#############################################################

cat("\n### Résumé des tests d’hétéroscédasticité ###\n")
cat("- Breusch-Pagan : ", ifelse(test_bp$p.value > 0.05, "Homoscédasticité", "Hétéroscédasticité"), "\n")
cat("- White : ", ifelse(white_pval > 0.05, "Homoscédasticité", "Hétéroscédasticité"), "\n")


########################################################
######## HYPOTHÈSE 4 : Diagnostic de l'autocorrélation #####
########################################################

library(car)
library(stats)
library(dplyr)
library(purrr)
library(lmtest)
library(forecast)
library(readxl)
library(tseries)

# Chargement des données
data_work4_log_out <- read_xlsx("//Users/mehdifehri/Desktop/R/Code Final Fec/data_work4_log_out.xlsx")

# Définition de la formule du modèle
variables_explicatives_ols <- setdiff(colnames(data_work4_log_out), c("Temps", "fec"))
formule_ols <- as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + ")))

# Ajustement du modèle OLS
model_final <- lm(formule_ols, data = data_work4_log_out)

# Extraction des résidus du modèle
residuals_final <- residuals(model_final)

########################################################
### Visualisation de l'autocorrélation des résidus  ###
########################################################

# Calcul des ACF et PACF sans plot direct
acf_res <- acf(residuals_final, plot = FALSE)
pacf_res <- pacf(residuals_final, plot = FALSE)

# Tableau ACF
acf_values <- data.frame(
  Lag = 1:length(acf_res$acf[-1]),  
  Autocorrelation = round(acf_res$acf[-1], 4) 
)

cat("\n--- Tableau des autocorrélations (ACF) ---\n")
print(acf_values)

# Tableau PACF
pacf_values <- data.frame(
  Lag = 1:length(pacf_res$acf),
  Partial_Autocorrelation = round(pacf_res$acf, 4)
)

cat("\n--- Tableau des autocorrélations partielles (PACF) ---\n")
print(pacf_values)

# Visualisation des ACF et PACF
plot(acf_res, main = "ACF des résidus", xlab = "Lags", ylab = "Autocorrélation")
plot(pacf_res, main = "PACF des résidus", xlab = "Lags", ylab = "Autocorrélation partielle")

########################################################
### Étape 1 : Test d'autocorrélation (Durbin-Watson) ###
########################################################

# Test de Durbin-Watson (autocorrélation d'ordre 1)
cat("\n--- Test Durbin-Watson ---\n")
dw_test <- tryCatch(durbinWatsonTest(model_final), error = function(e) NULL)
print(dw_test)
str(dw_test)

# Interprétation standard
# DW = 2 => Pas d'autocorrélation
# DW < 2 => Autocorrélation positive
# DW > 2 => Autocorrélation négative

if (!is.null(dw_test)) {
  dw_stat <- dw_test$dw    # Extraction de la statistique Durbin-Watson
  dw_p_value <- dw_test$p  # Extraction de la p-value
  
  cat("Durbin-Watson Statistic :", round(dw_stat, 4), "\n")
  cat("p-value :", round(dw_p_value, 4), "\n")
  
  if (!is.na(dw_stat)) {
    if (dw_stat < 2) {
      cat("Conclusion : Les résidus suggèrent une autocorrélation positive.\n")
    } else if (dw_stat > 2) {
      cat("Conclusion : Les résidus suggèrent une autocorrélation négative.\n")
    } else {
      cat("Conclusion : Les résidus ne présentent pas d'autocorrélation significative d'ordre 1.\n")
    }
  } else {
    cat("La statistique Durbin-Watson est indisponible (NA).\n")
  }
  
} else {
  cat("Le test Durbin-Watson n'a pas pu être exécuté.\n")
}


########################################################
### Test d'autocorrélation d'ordre 40 (Breusch-Godfrey) #
########################################################

cat("\n--- Test d'autocorrélation d'ordre 40 (Breusch-Godfrey) ---\n")

bg_test <- tryCatch({
  bgtest(model_final, order = 40)
}, error = function(e) {
  cat("Erreur lors de l'exécution du test de Breusch-Godfrey : ", e$message, "\n")
  return(NULL)
})

if (!is.null(bg_test)) {
  # Afficher tous les détails de l'objet bg_test
  print(bg_test)
  
  # Extraction des résultats
  bg_stat <- bg_test$statistic
  bg_p_value <- bg_test$p.value
  bg_df <- bg_test$parameter  # Degrés de liberté
  
  # Affichage détaillé
  cat("\n--- Détails du test de Breusch-Godfrey ---\n")
  cat("Statistique du test :", round(bg_stat, 4), "\n")
  cat("Degrés de liberté :", bg_df, "\n")
  cat("P-value :", round(bg_p_value, 4), "\n")
  
  # Interprétation du test
  # H0 : Pas d'autocorrélation jusqu'à l'ordre spécifié
  # H1 : Présence d'autocorrélation jusqu'à l'ordre spécifié
  if (bg_p_value > 0.05) {
    cat("Conclusion : Pas d'autocorrélation significative jusqu'à l'ordre 40 (on ne rejette pas H0).\n")
  } else {
    cat("Conclusion : Autocorrélation significative détectée jusqu'à l'ordre 40 (on rejette H0).\n")
  }
  
  # Ajout d'une recommandation pour améliorer le modèle
  cat("\n--- Recommandation ---\n")
  if (bg_p_value <= 0.05) {
    cat("Il est conseillé d'ajuster un modèle prenant en compte cette autocorrélation, par exemple via un modèle ARIMA ou une correction robuste des erreurs standard.\n")
  } else {
    cat("Le modèle semble adapté du point de vue de l'autocorrélation.\n")
  }
} else {
  cat("Le test Breusch-Godfrey n'a pas pu être exécuté.\n")
}


########################################################
### Test d'autocorrélation de Ljung-Box jusqu'à 40 lags #
########################################################

cat("\n--- Test d'autocorrélation (Ljung-Box) jusqu'à 40 lags ---\n")

max_lags <- 40
ljung_box_results <- lapply(1:max_lags, function(lag) {
  test <- Box.test(residuals_final, lag = lag, type = "Ljung-Box")
  data.frame(
    Lag = lag,
    Statistic = round(test$statistic, 4),
    P_Value = round(test$p.value, 4),
    Significant = test$p.value <= 0.05
  )
}) %>% bind_rows()

print(ljung_box_results)

# Résumé des lags significatifs
significant_lags <- ljung_box_results %>% filter(Significant == TRUE)
if (nrow(significant_lags) > 0) {
  cat("\nLags avec autocorrélation significative (Ljung-Box) :\n")
  print(significant_lags)
  cat("Conclusion : Il existe une autocorrélation résiduelle à certains retards.\n")
} else {
  cat("\nAucune autocorrélation significative détectée jusqu'à 40 lags (Ljung-Box).\n")
  cat("Conclusion : Les résidus ne présentent pas d'autocorrélation significative.\n")
}

########################################################
### Vérification de la saisonnalité et stationnarité ###
########################################################

cat("\n--- Test de saisonnalité et stationnarité ---\n")

#
ts_residuals <- ts(residuals_final, frequency = 4)

# Test de saisonnalité
cat("\n--- Vérification de la saisonnalité ---\n")
seasonality_test <- kruskal.test(ts_residuals ~ cycle(ts_residuals))
cat("Statistique du test Kruskal-Wallis :", round(seasonality_test$statistic, 4), "\n")
cat("P-value :", round(seasonality_test$p.value, 4), "\n")

if (seasonality_test$p.value < 0.05) {
  cat("Conclusion : Les résidus montrent une saisonnalité significative.\n")
} else {
  cat("Conclusion : Pas de saisonnalité significative détectée.\n")
}

# Test de stationnarité : Dickey-Fuller Augmenté (ADF)
cat("\n--- Vérification de la stationnarité (ADF Test) ---\n")
adf_test <- adf.test(ts_residuals, alternative = "stationary")
cat("Statistique ADF :", round(adf_test$statistic, 4), "\n")
cat("P-value :", round(adf_test$p.value, 4), "\n")

if (adf_test$p.value < 0.05) {
  cat("Conclusion : Les résidus sont stationnaires (on rejette H0).\n")
} else {
  cat("Conclusion : Les résidus ne sont pas stationnaires (on ne rejette pas H0).\n")
}

# Test de stationnarité : KPSS
cat("\n--- Vérification de la stationnarité (KPSS Test) ---\n")
kpss_test <- kpss.test(ts_residuals)
cat("Statistique KPSS :", round(kpss_test$statistic, 4), "\n")
cat("P-value :", round(kpss_test$p.value, 4), "\n")

if (kpss_test$p.value > 0.05) {
  cat("Conclusion : Les résidus sont stationnaires selon le test KPSS (on ne rejette pas H0).\n")
} else {
  cat("Conclusion : Les résidus ne sont pas stationnaires selon le test KPSS (on rejette H0).\n")
}


########################################################
### Ajustement d'un modèle ARIMA(4,0,0) sur les résidus ###
########################################################

cat("\n--- Ajustement du modèle ARIMA(4,0,0) ---\n")

# Ajustement du modèle ARIMA(4,0,0)
arima_model_400 <- arima(
  residuals_final,
  order = c(4, 0, 0),  # ARIMA(p=4, d=0, q=0)
  include.mean = TRUE  # Inclure une constante si nécessaire
)

# Résumé du modèle ARIMA
cat("\nRésumé du modèle ARIMA(4,0,0) :\n")
print(summary(arima_model_400))

# Extraction des résidus corrigés
arima_residuals <- residuals(arima_model_400)

########################################################
### Vérification des résidus corrigés ###
########################################################

cat("\n--- Vérification des résidus corrigés (ARIMA) ---\n")

# ACF et PACF des résidus corrigés
acf(arima_residuals, main = "ACF des résidus corrigés")
pacf(arima_residuals, main = "PACF des résidus corrigés")

# Test de Ljung-Box sur les résidus corrigés
cat("\n--- Test de Ljung-Box sur les résidus corrigés ---\n")
ljung_box_arima <- Box.test(arima_residuals, lag = 40, type = "Ljung-Box")

cat("Statistique Ljung-Box :", round(ljung_box_arima$statistic, 4), "\n")
cat("P-value :", round(ljung_box_arima$p.value, 4), "\n")

if (ljung_box_arima$p.value > 0.05) {
  cat("Conclusion : Les résidus du modèle ARIMA ne présentent pas d'autocorrélation significative.\n")
} else {
  cat("Conclusion : Les résidus présentent encore une autocorrélation significative. Une révision du modèle ARIMA est nécessaire.\n")
}

# Test de Durbin-Watson sur les résidus corrigés
cat("\n--- Test Durbin-Watson sur les résidus corrigés ---\n")

dw_test_corrected <- durbinWatsonTest(lm(arima_residuals ~ 1))

# Affichage des résultats
print(dw_test_corrected)

# Interprétation de la p-value
cat("\nInterprétation :\n")
if (dw_test_corrected$p > 0.05) {
  cat("H0 acceptée : Pas d'autocorrélation d'ordre 1 détectée.\n")
} else {
  cat("H0 rejetée : Présence d'autocorrélation d'ordre 1 détectée.\n")
}

########################################################
### Prévisions avec le modèle ARIMA ###
########################################################

# Prévisions sur 10 périodes
cat("\n--- Prévisions avec le modèle ARIMA ---\n")
forecast_results <- forecast(arima_model, h = 10)  # h = nombre de périodes à prévoir
print(forecast_results)

# Visualisation des prévisions
plot(forecast_results, main = "Prévisions avec ARIMA", col = "blue")

########################################################
### Étape 2 : Création du dataframe avec résidus corrigés ###
########################################################

cat("\n--- Création du nouveau dataframe avec résidus corrigés ---\n")
# Créer un nouveau dataframe avec les résidus corrigés
data_work_final_auto <- data_work4_log_out
data_work_final_auto$residuals_corriges <- arima_residuals

# Affichage des premières lignes du dataframe
print(head(data_work_final_auto))

########################################################
### Étape 3 : Nouvelle régression OLS ###
########################################################

cat("\n--- Nouvelle régression OLS avec données corrigées ---\n")

# Modèle avec les nouvelles données corrigées
formule_ols <- as.formula(paste("fec ~", paste(variables_explicatives_ols, collapse = " + ")))
model_final_corrige <- lm(formule_ols, data = data_work_final_auto)

# Résumé du nouveau modèle OLS
summary_ols_corrige <- summary(model_final_corrige)
print(summary_ols_corrige)

# Comparaison des coefficients
cat("\n--- Comparaison des coefficients OLS avant et après correction ---\n")
coeff_ancien <- coef(model_final)  # Coefficients du modèle initial
coeff_corrige <- coef(model_final_corrige)  # Coefficients du modèle corrigé

# Création d'un tableau comparatif
comparaison_coeff <- data.frame(
  Coefficients_anciens = coeff_ancien,
  Coefficients_corriges = coeff_corrige
)
print(comparaison_coeff)

### Vérification des hypothèses d'homoscédasticité du nouveau modèle ###

library(lmtest)

cat("\n--- Test de Breusch-Pagan pour l'homoscédasticité ---\n")

# Test de Breusch-Pagan sur le nouveau modèle OLS
bp_test <- bptest(model_final_corrige)

# Affichage des résultats
cat("Statistique du test Breusch-Pagan :", round(bp_test$statistic, 4), "\n")
cat("Degrés de liberté :", bp_test$parameter, "\n")
cat("P-value :", round(bp_test$p.value, 4), "\n")

# Interprétation
if (bp_test$p.value > 0.05) {
  cat("Conclusion : H0 acceptée. Les résidus sont homoscédastiques.\n")
} else {
  cat("Conclusion : H0 rejetée. Les résidus présentent une hétéroscédasticité significative.\n")
}

################################
################# FIN ######################
########################

