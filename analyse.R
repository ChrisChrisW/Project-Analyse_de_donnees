# Nettoyage de l'environnement
rm(list=ls())
graphics.off()

# ----------------------------------------------------------------
# --------------- CHARGEMENT DES DONNÉES -------------------------
# ----------------------------------------------------------------

# Chargement des bibliothèques
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("skimr")) install.packages("skimr")
if (!require("VIM")) install.packages("VIM")
if (!require("outliers")) install.packages("outliers")

# Chargement des bibliothèques
library("ggplot2")    # Bibliothèque pour les graphiques
library(skimr)        # Bibliothèque pour les statistiques descriptives
library(VIM)          # Bibliothèque pour la gestion des données manquantes
library(outliers)     # Bibliothèque pour la détection des valeurs aberrantes

# Chargement des Données
tab <- read.csv(file="./data/credit-card-customers/cleaned_data.csv",
                sep=',',
                dec='.',
                header=TRUE)

# ----------------------------------------------------------------
# Objectif 1: Compréhension des caractéristiques démographiques
# ----------------------------------------------------------------
# Profils démographiques des clients résiliant leurs services de cartes de crédit
par(mfrow = c(2, 3))

# Analyse univariée pour chaque variable démographique
for (var in c("Customer_Age", "Gender", "Dependent_count", "Education_Level", "Marital_Status", "Income_Category")) {
  barplot(table(tab$Attrition_Flag, tab[[var]]), main = var, col = c("skyblue", "lightblue"), legend.text = TRUE)
}

# ----------------------------------------------------------------
# Objectif 2: Étude des relations entre les variables
# ----------------------------------------------------------------
# Matrice de corrélation pour les variables démographiques et transactionnelles
correlation_matrix <- cor(tab[, c("Customer_Age", "Dependent_count", "Credit_Limit", "Total_Trans_Amt", "Total_Trans_Ct")])

# Affichage de la matrice de corrélation
print(correlation_matrix)

# Test de corrélation entre variables
cor.test(tab$Customer_Age, tab$Total_Trans_Amt)

# ----------------------------------------------------------------
# Objectif 3: Facteurs sous-jacents à la résiliation
# ----------------------------------------------------------------

# Sélection des variables pertinentes pour l'analyse factorielle
selected_vars <- c("Customer_Age", "Credit_Limit", "Total_Trans_Amt")

# Affichage de la structure des colonnes sélectionnées
str(tab[, selected_vars])

# Résumé statistique des colonnes sélectionnées
summary(tab[, selected_vars])

# Analyse factorielle pour réduire la dimensionnalité des données
nb_facteurs <- 2  # Nombre de facteurs à extraire
fact_model <- fa(tab[, selected_vars], nfactors = nb_facteurs, rotate = "varimax")

# Affichage des résultats de l'analyse factorielle
print(fact_model)

# ----------------------------------------------------------------
# Objectif 4: Modélisation prédictive
# ----------------------------------------------------------------

# Transformation de la variable Attrition_Flag en binaire
tab$Attrition_Flag <- ifelse(tab$Attrition_Flag == "Attrited Customer", 1, 0)

# Vérification des valeurs uniques de la variable Attrition_Flag
unique(tab$Attrition_Flag)

# Modèle de régression logistique, prédictive pour estimer la probabilité de résiliation
model <- glm(Attrition_Flag ~ Customer_Age + Dependent_count + Credit_Limit + Total_Trans_Amt + Total_Trans_Ct, data = tab, family = "binomial")

# Affichage des résultats du modèle
summary(model)

# ----------------------------------------------------------------
# Objectif 5: Classification des clients
# ----------------------------------------------------------------
# Utilisation de méthodes de classification (par exemple, k-means)
set.seed(123)  # Pour la reproductibilité
num_clusters <- 3
cluster_vars <- tab[, c("Customer_Age", "Credit_Limit", "Total_Trans_Amt", "Total_Trans_Ct")]

# Algorithme de k-means
kmeans_model <- kmeans(cluster_vars, centers = num_clusters, nstart = 20)

# Affichage des résultats de la classification
print(kmeans_model)

# ----------------------------------------------------------------
# Visualisation des résultats de la classification
# ----------------------------------------------------------------
# Visualisation en 2D des clusters créés par k-means
library(cluster)
par(mfrow = c(1, 1))

clusplot(cluster_vars, kmeans_model$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)