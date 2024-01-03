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
if (!require("psych")) install.packages("psych")
if (!require("corrplot")) install.packages("corrplot")
if (!require("cluster")) install.packages("cluster")

# Chargement des bibliothèques
library("ggplot2")    # Bibliothèque pour les graphiques
library(skimr)        # Bibliothèque pour les statistiques descriptives
library(VIM)          # Bibliothèque pour la gestion des données manquantes
library(outliers)     # Bibliothèque pour la détection des valeurs aberrantes
library(psych)        # Bibliothèque pour les analyses psychologiques et statistiques
library(corrplot)     # Bibliothèque pour les graphiques de matrices de corrélation
library(cluster)      # Bibliothèque pour les analyses de regroupement (clustering)

# Chargement des Données
tab <- read.csv(file="./data/credit-card-customers/cleaned_data.csv",
                sep=',',
                dec='.',
                header=TRUE)

# ----------------------------------------------------------------
# Objectif 1: Compréhension des caractéristiques démographiques
# ----------------------------------------------------------------
# Calcul des pourcentages
calculate_percentage <- function(x, y) {
  return(prop.table(table(x, y), margin = 1) * 100)
}

# Profils démographiques des clients résiliant leurs services de cartes de crédit
for (var in c("Customer_Age", "Gender", "Dependent_count", "Education_Level", "Marital_Status", "Income_Category")) {
  # Analyse univariée pour chaque variable démographique
  cat("====================\n")
  cat("Variable:", var, "\n\n")
  
  cat("Tableau des fréquences:\n")
  print(table(tab$Attrition_Flag, tab[[var]]))
  
  cat("\nTableau des fréquences avec pourcentages:\n")
  print(calculate_percentage(tab$Attrition_Flag, tab[[var]]))
  cat("====================\n")
}

par(mfrow = c(2, 3))

# Analyse univariée pour chaque variable démographique
for (var in c("Customer_Age", "Gender", "Dependent_count", "Education_Level", "Marital_Status", "Income_Category")) {
  barplot(calculate_percentage(tab$Attrition_Flag, tab[[var]]), main = var, col = c("skyblue", "lightblue"), legend.text = TRUE)
}

# ----------------------------------------------------------------
# Objectif 2: Étude des relations entre les variables
# ----------------------------------------------------------------
# Variables à tester
variables <- c("Customer_Age", "Dependent_count", "Credit_Limit", "Total_Trans_Amt", "Total_Trans_Ct")

# Matrice de corrélation pour les variables démographiques et transactionnelles
correlation_matrix <- cor(tab[, variables])

# Affichage de la matrice de corrélation
print(correlation_matrix)

# Plot avec corrplot
par(mfrow = c(1, 1))
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# Boucle pour les tests de corrélation
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    
    # Exclure la paire "Total_Trans_Amt" et "Total_Trans_Ct"
    if (!(variables[i] %in% c("Total_Trans_Amt", "Total_Trans_Ct") && variables[j] %in% c("Total_Trans_Amt", "Total_Trans_Ct"))) {
      
      cat("Corrélation entre", variables[i], "et", variables[j], ":\n")
      
      # Test de corrélation
      result <- cor.test(tab[, variables[i]], tab[, variables[j]])
      
      # Affichage de la p-value
      cat("P-value =", result$p.value, "\n\n")
    }
  }
}

# ----------------------------------------------------------------
# Objectif 3: Facteurs sous-jacents à la résiliation
# ----------------------------------------------------------------
# Sélection des variables pertinentes pour l'analyse factorielle
selected_vars <- c("Customer_Age", "Credit_Limit", "Total_Trans_Amt", "Attrition_Flag")

# Sélection des variables pertinentes
selected_data <- tab[, selected_vars]

# Conversion de la variable Attrition_Flag en facteur
selected_data$Attrition_Flag <- as.factor(selected_data$Attrition_Flag)

# Conversion du facteur en numérique (0 pour Existing Customer, 1 pour Attrited Customer)
selected_data$Attrition_Flag <- as.numeric(selected_data$Attrition_Flag) - 1

# Affichage de la structure des colonnes sélectionnées
str(tab[, selected_vars])

# Analyse factorielle pour réduire la dimensionnalité des données
eigen_values <- eigen(cor(selected_data))$values # Critère de Kaiser-Guttman - retenir que les valeurs propres sup à 1
nb_facteurs <- sum(eigen_values > 1) # Nombre de facteurs à extraire

# Réalisation de l'analyse factorielle avec la variable Attrition_Flag
fact_model <- fa(selected_data, nfactors = nb_facteurs, rotate = "varimax")

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

cluster_vars <- tab[, c("Customer_Age", "Credit_Limit", "Total_Trans_Amt", "Total_Trans_Ct")]


# ----------------------------------------------------------------
# Méthode du coude
# ----------------------------------------------------------------

# 1 - Trouver le nombre optimal de clusters (méthode du coude)

# Fonction pour calculer la somme totale des carrés au sein des clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(cluster_vars, k)
  return (cluster$tot.withinss)
}

# Exécuter l'algorithme sur une plage de valeurs de k
max_k <- 20
wss <- sapply(2:max_k, kmean_withinss)

# Construire un dataframe pour stocker les résultats
elbow <- data.frame(K = 2:max_k, WSS = wss)

# Afficher le dataframe pour voir les résultats
print(elbow)

# 2 - Tracer le graphique du coude

# Tracer le graphique du coude avec une ligne verticale pour le nombre optimal de clusters
plot(elbow$K, elbow$WSS, type = "b", pch = 19, frame = FALSE, col = "blue",
     xlab = "Nombre de clusters (K)", ylab = "Somme totale des carrés au sein des clusters")

# 3 - Exécuter l'algorithme K-means avec le nombre optimal de clusters

# Choisissez le nombre optimal de clusters (à partir du coude)
optimal_k <- elbow$K[which.max(elbow$WSS)]

# Ajouter une ligne verticale pour le nombre optimal de clusters
abline(v = optimal_k, col = "red", lty = 2)

# Légende avec l'abréviation de WSS
legend("topright", legend = c("WSS (Somme totale des carrés intra-clusters)", "Optimal K"), col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 2))

# Exécuter l'algorithme K-means avec le nombre optimal de clusters
kmeans_model <- kmeans(cluster_vars, optimal_k)

# Afficher les résultats
print(kmeans_model$cluster)
print(kmeans_model$centers)
print(kmeans_model$size)



# ----------------------------------------------------------------
# Méthode silhouette
# ----------------------------------------------------------------

# Fonction pour calculer la silhouette moyenne
calculate_avg_silhouette <- function(data, centers) {
  kmeans_model <- kmeans(data, centers = centers, nstart = 42)
  cluster_assignments <- kmeans_model$cluster
  silhouette_avg <- silhouette(cluster_assignments, dist(data))
  return(mean(silhouette_avg[, "sil_width"]))
}

# Test de différentes valeurs de clusters
num_clusters <- 2:20  # Vous pouvez ajuster la plage selon votre besoin
# Test de différentes valeurs de clusters avec suppression des avertissements
suppressWarnings({
  silhouette_values <- sapply(num_clusters, function(k) calculate_avg_silhouette(cluster_vars, k))
})

# Tracé du graphique pour la méthode de la silhouette
plot(num_clusters, silhouette_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Nombre de clusters", ylab = "Silhouette moyenne",
     main = "Méthode de la silhouette pour déterminer le nombre optimal de clusters")

# Sélection du meilleur nombre de clusters
best_num_clusters <- num_clusters[which.max(silhouette_values)]
cat("Le meilleur nombre de clusters est:", best_num_clusters, "\n")

# Algorithme de k-means sur le meilleur nombre de clusters
kmeans_model <- kmeans(cluster_vars, centers = best_num_clusters, nstart = 42)
abline(v = best_num_clusters, col = "red", lty = 2)
legend("topright", legend = paste("Coude potentiel à k =", best_num_clusters), col = "red", lty = 2)

# Affichage des résultats de la classification
print(kmeans_model)

# ----------------------------------------------------------------
# Visualisation des résultats de la classification
# ----------------------------------------------------------------
# Visualisation en 2D des clusters créés par k-means
par(mfrow = c(1, 1))
clusplot(cluster_vars, kmeans_model$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Sans les points de données
clusplot(cluster_vars, kmeans_model$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="Clusters", col.p="transparent")