# credit revolving : https://www.service-public.fr/particuliers/vosdroits/F2436

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
tab <- read.csv(file="./data/credit-card-customers/BankChurners.csv",
                sep=',',
                dec='.',
                header=TRUE)

# ----------------------------------------------------------------
# ------------ EXPLORATION DES DONNÉES --------------------------
# ----------------------------------------------------------------

# Afficher les premières lignes du dataframe
head(tab)

# Résumé statistique des variables numériques
summary(tab)

# Information sur les types de données et les valeurs manquantes
str(tab)

# Vérification de la taille et des noms de colonnes
colnames(tab)
dim(tab)

# Classe du tableau
class(tab)

# ----------------------------------------------------------------
# DÉTECTION DES VALEURS MANQUANTES ET VISUALISATION -------------
# ----------------------------------------------------------------

# Détecter les valeurs manquantes
if (sum(is.na(tab)) > 0) {
  print(tab[is.na(tab)])
  stop("Des valeurs NaN ont été détectées. Veuillez nettoyer les données avant de continuer.")
}

# Visualisation des valeurs manquantes
par(mfrow = c(1, 1))
aggr(tab, col = c("navajowhite1", "navajowhite3"), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(tab), 
     cex.axis = 0.7, 
     gap = 3, 
     pch = 19)

# ----------------------------------------------------------------
# DÉTECTION DES VALEURS ABERANTES --------------------------------
# ----------------------------------------------------------------

# Liste pour stocker les outliers détectés pour chaque colonne
all_outliers <- list()

# Détection des valeurs aberrantes pour chaque colonne
numeric_columns <- sapply(tab, is.numeric)

for (col in colnames(numeric_columns)) {
  outliers_test <- grubbs.test(tab[[col]])
  outliers_detected <- outliers_test$outliers
  all_outliers[[col]] <- outliers_detected
}

# Afficher les valeurs aberrantes détectées pour chaque colonne
print(all_outliers)

# ----------------------------------------------------------------
# ANALYSE DES VARIABLES QUANTITATIVES ---------------------------
# ----------------------------------------------------------------

# Variables quantitatives
quantitative_vars <- c("CLIENTNUM", "Customer_Age", "Dependent_count", "Months_on_book", "Total_Relationship_Count",
                       "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", "Total_Revolving_Bal",
                       "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt", "Total_Trans_Ct",
                       "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio")

# Résumé pour les variables quantitatives
summaries_data <- summary(tab[, quantitative_vars])

# Tableaux de fréquence pour les variables catégorielles
tables_data <- lapply(c("Attrition_Flag", "Gender", "Education_Level", "Marital_Status", "Income_Category", "Card_Category"),
                      function(x) table(tab[[x]]))

# Affichage des résumés et tableaux de fréquence
summaries_data
tables_data

# Boîtes à moustaches pour les variables quantitatives
par(mfrow = c(1, 1))

for (var in quantitative_vars) {
  boxplot(tab[[var]], main = var, col = "skyblue", border = "black")
}

# Histogrammes pour les variables catégorielles
par(mfrow = c(1, 1))

for (variable_name in names(tables_data)) {
  barplot(tables_data[[variable_name]], main = variable_name, col = "lightblue")
}


# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

# Regarder les valeurs possiblement abérantes sur le boxplot
quantitative_vars_to_check <- c("Credit_Limit","Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1",
                                "Total_Trans_Ct","Total_Ct_Chng_Q4_Q1")
tables_data_to_check <- list(
  Customer_Age = table(tab$Customer_Age),
  Months_on_book = table(tab$Months_on_book),
  Months_Inactive_12_mon = table(tab$Months_Inactive_12_mon),
  Contacts_Count_12_mon = table(tab$Contacts_Count_12_mon),
  Credit_Limit = table(tab$Credit_Limit),
  Avg_Open_To_Buy = table(tab$Avg_Open_To_Buy),
  Total_Amt_Chng_Q4_Q1 = table(tab$Total_Amt_Chng_Q4_Q1),
  Total_Trans_Amt = table(tab$Total_Trans_Amt),
  Total_Trans_Ct = table(tab$Total_Trans_Ct),
  Total_Ct_Chng_Q4_Q1 = table(tab$Total_Ct_Chng_Q4_Q1)
)

for (variable_name in names(tables_data_to_check)) {
  barplot(tables_data_to_check[[variable_name]], main=variable_name, col="lightblue")
}

# Visions de Outliers

# Customer_Age : Aucun problème évident n'est détecté, bien que l'on observe un léger dépassement sur le boxplot, suggérant la nécessité d'une inspection plus approfondie.
# Months_on_book : Aucun problème majeur n'est apparent, probablement lié à un ajustement de calibrage. Il est recommandé de consulter le résumé pour une confirmation supplémentaire
# Months_Inactive_12_mon : Aucun problème significatif n'est observé, probablement dû à un calibrage particulier. Une vérification du résumé peut fournir des détails supplémentaires.
# Contacts_Count_12_mon : Des observations similaires sont notées, indiquant probablement un ajustement de calibrage.
# Credit_Limit : Après l'analyse du barplot, les données semblent propres et homogènes.
# Avg_Open_To_Buy : L'analyse du barplot révèle deux pics importants, mais la nature de la donnée ne semble pas aberrante.

# Total_Amt_Chng_Q4_Q1 (Valeurs Aberrantes : 3.397, 3.355) : Malgré une impression de distribution normale d'après le barplot, l'étude des quartiles identifie deux valeurs aberrantes.
qqnorm(tab$Total_Amt_Chng_Q4_Q1)
qqline(tab$Total_Amt_Chng_Q4_Q1)

# Total_Trans_Amt : Aucune valeur aberrante flagrante n'est détectée après l'analyse du barplot.
# Total_Trans_Ct : Bien que le barplot suggère une distribution normale, l'étude des quartiles ne confirme pas de manière concluante la présence de valeurs aberrantes.
qqnorm(tab$Total_Trans_Ct)
qqline(tab$Total_Trans_Ct)
# Total_Ct_Chng_Q4_Q1 : L'analyse du barplot, combinée à la nature de la variable, indique qu'il n'est pas possible d'exclure les changements dans le nombre de transactions comme des valeurs aberrantes.


# ----------------------------------------------------------------
# TRAITEMENT DES VALEURS INCONNUES --------------------------------
# ----------------------------------------------------------------

# Supprimer les lignes avec des valeurs "Unknown"
tab <- tab[rowSums(tab == "Unknown", na.rm = TRUE) == 0, ]

# Vérifier les dimensions du dataset après la suppression
tables_data_after_cleaning <- lapply(
  c("Attrition_Flag", "Gender", "Education_Level", 
    "Marital_Status", "Income_Category", "Card_Category"),
                                     function(x) table(tab[[x]]))
list(tables_data_after_cleaning)

# ----------------------------------------------------------------
# TRAITEMENT DES VALEURS ABERRANTES ------------------------------
# ----------------------------------------------------------------
tab <- tab[!(tab$Total_Amt_Chng_Q4_Q1 %in% c(3.397, 3.355)), ]

# ----------------------------------------------------------------
# CORRECTION D'UNE VALEUR FLOAT ---------------------------------
# ----------------------------------------------------------------

# Remplacer la seule valeur float en valeur int
tab$Avg_Open_To_Buy[tab$Avg_Open_To_Buy == 1438.3] <- 1438

# Afficher les lignes avec des valeurs de Avg_Open_To_Buy égale à 1438.3
rows_greater_than_1438 <- tab[tab$Avg_Open_To_Buy == 1438.3, ]
print(rows_greater_than_1438$Avg_Open_To_Buy)

summary(tab$Avg_Open_To_Buy)

# ----------------------------------------------------------------
# ANALYSE DE LA DISTRIBUTION DES VARIABLES ----------------------
# ----------------------------------------------------------------

# TODO

# ----------------------------------------------------------------
# VARIABLES DISCRETES ET CONTINUES ----------------------------------------
# ----------------------------------------------------------------

# Variables discretes
discrete_vars <- c("CLIENTNUM", "Attrition_Flag", "Gender", "Dependent_count",
                   "Education_Level", "Marital_Status", "Income_Category",
                   "Card_Category", "Months_on_book", "Months_Inactive_12_mon",
                   "Contacts_Count_12_mon")

# Variables continues
continuous_vars <- c("Customer_Age", "Credit_Limit", "Total_Relationship_Count",
                     "Total_Revolving_Bal", "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1",
                     "Total_Trans_Amt", "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1",
                     "Avg_Utilization_Ratio")

# Visualisations pour les variables discrètes
for (var in discrete_vars) {
  print(
    ggplot(tab, aes(x = as.factor(get(var)))) +
      geom_bar() +
      labs(title = paste("Bar Plot for", var),
           x = var, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# Visualisations pour les variables continues
for (var in continuous_vars) {
  print(
    ggplot(tab, aes(x = get(var))) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram for", var),
           x = var, y = "Frequency") +
      theme_minimal()
  )
}

# ----------------------------------------------------------------
# ANALYSE DES STATISTIQUES DESCRIPTIVES --------------------------
# ----------------------------------------------------------------

skim(tab[, -c((ncol(tab) - 1):ncol(tab))])


# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------


# A réfléchir : Supprimer les lignes avec "Attrited Customer" de tab qui correspond aux comptes résiliés
# tab <- tab[tab$Attrition_Flag != "Attrited Customer", ]


# ----------------------------------------------------------------
# EXPORTATION DES DONNÉES NETTOYÉES EN CSV
# ----------------------------------------------------------------
write.csv(tab, file = "./data/credit-card-customers/cleaned_data.csv", row.names = FALSE)
