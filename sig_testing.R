library(tidyverse)
library(skimr)
# Perform statistical tests for each variable
data <- read.csv("kproto_complete_data.csv")
skim(data)
colnames(data[,3:56])
# Create an empty dataframe to store results
results_df <- tibble(Variable = character(), P_Value = numeric())

# Specify categorical variable names
categorical_vars <- c("gender") 

# Specify continuous variable names
continuous_vars <- colnames(data[,3:56])

# Perform statistical tests for each categorical variable
for (var in categorical_vars) {
  cross_tab <- table(data$Cluster4, data[[var]])
  chi_sq <- chisq.test(cross_tab)
  results_df <- rbind(results_df, tibble(Variable = var, P_Value = chi_sq$p.value))
}

# Perform statistical tests for each continuous variable
for (var in continuous_vars) {
  kruskal_wallis <- kruskal.test(data[[var]] ~ data$Cluster4)
  results_df <- rbind(results_df, tibble(Variable = var, P_Value = kruskal_wallis$p.value))
}

# Print the results dataframe
print(results_df)
