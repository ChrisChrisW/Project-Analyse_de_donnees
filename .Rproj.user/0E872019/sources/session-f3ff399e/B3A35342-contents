# cleaner
rm(list=ls()); graphics.off()

# imports
library("ggplot2")

# Read the dataset
tab <- read.csv(file="./data/credit-card-customers/BankChurners.csv",sep=',',dec='.',header=TRUE)

# check size and colonne names
colnames(tab)
dim(tab)

# tab class -> data.frame
class(tab)

# Dectect NaN values
if (sum(is.na(tab)) > 0) {
  print(tab[is.na(tab)])
  stop("NaN values detected. Please clean the data before proceeding.")
}

# Summary for quantitative variables
quantitative_vars <- c("CLIENTNUM", "Customer_Age", "Dependent_count", "Months_on_book", "Total_Relationship_Count",
                       "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", "Total_Revolving_Bal",
                       "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Amt", "Total_Trans_Ct",
                       "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio")
summaries_data <- summary(tab[, quantitative_vars])

# Frequency table for categorical variables
tables_data <- list(
  Attrition_Flag = table(tab$Attrition_Flag),
  Gender = table(tab$Gender),
  Education_Level = table(tab$Education_Level),
  Marital_Status = table(tab$Marital_Status),
  Income_Category = table(tab$Income_Category),
  Card_Category = table(tab$Card_Category)
)

# summary or table
summaries_data
tables_data



# Boxplots for Quantitative Variables
par(mfrow=c(1, 1))  # Adjust the grid based on your preference

for (var in quantitative_vars) {
  boxplot(tab[, var], main=var, col="skyblue", border="black")
}

# Histograms for Categorical Variables
par(mfrow=c(1, 1))  # Adjust the grid based on your preference

for (variable_name in names(tables_data)) {
  barplot(tables_data[[variable_name]], main=variable_name, col="lightblue")
}


# TODO : put in histo to check not good value
# Total_Ct_Chng_Q4_Q1, Total_Trans_Ct, Total_Amt_Chng_Q4_Q1, Avg_Open_To_Buy, Credit_Limit, 
quantitative_vars_to_check <- c("Credit_Limit","Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1", "Total_Trans_Ct","Total_Ct_Chng_Q4_Q1")
tables_data_to_check <- list(
  Credit_Limit = table(tab$Credit_Limit),
  Avg_Open_To_Buy = table(tab$Avg_Open_To_Buy),
  Total_Amt_Chng_Q4_Q1 = table(tab$Total_Amt_Chng_Q4_Q1),
  Total_Trans_Ct = table(tab$Total_Trans_Ct),
  Total_Ct_Chng_Q4_Q1 = table(tab$Total_Ct_Chng_Q4_Q1)
)
tables_data_to_check
for (variable_name in names(tables_data_to_check)) {
  barplot(tables_data_to_check[[variable_name]], main=variable_name, col="lightblue")
}

# TODO : after evaluation, check Avg_Open_To_Buy, Total_Amt_Chng_Q4_Q1, Total_Amt_Chng_Q4_Q1



# TODO : delete Unknow value


# credit revolving : https://www.service-public.fr/particuliers/vosdroits/F2436









