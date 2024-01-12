# Credit Card Customer Analysis and Classification

## Project Overview

This project focuses on analyzing and classifying credit card customers based on their demographic attributes and transactional behavior. The main objectives include understanding demographic characteristics, exploring relationships between variables, identifying underlying factors contributing to customer attrition, building a predictive logistic regression model, and classifying customers using the k-means clustering method.

## Project Structure

### 1. Environment Setup

   Before diving into the analysis, the R environment is cleaned and necessary libraries are loaded.

### 2. Data Loading

   The project utilizes credit card customer data, loaded from the "cleaned_data.csv" file.

### 3. Demographic Profiling

   The demographic profiles of customers who terminated their credit card services are examined. This involves calculating percentages and creating bar plots to visualize demographic characteristics.

### 4. Variable Relationships

   Exploratory analysis includes studying relationships between key variables, such as customer age, dependent count, credit limit, total transaction amount, and total transaction count. Correlation matrices and tests are conducted.

### 5. Factors Influencing Attrition

   An exploration into the factors underlying customer attrition is performed. This involves selecting relevant variables, converting categorical data, and conducting a factor analysis to reduce data dimensionality.

### 6. Predictive Modeling

   Logistic regression is employed to build a predictive model estimating the probability of customer attrition. Model results and insights are presented.

### 7. Customer Classification

   The clustering method k-means is applied to classify customers into distinct groups based on selected features. The optimal number of clusters is determined using both the elbow method and silhouette method.

### 8. Results Visualization

   The results of the k-means clustering are visualized in 2D plots for better understanding and interpretation.

## Getting Started

To explore the project:

1. Clone the repository.
2. Open the R script in your preferred R environment.
3. Run each section to execute the analysis step by step.

- **R**:
   - [Download R](https://cran.r-project.org/mirrors.html)

- **RStudio**: RStudio is an integrated development environment (IDE) for R.
   - [Download RStudio](https://www.rstudio.com/products/rstudio/download/)
