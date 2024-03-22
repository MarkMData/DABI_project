library(tidyverse)
library(skimr)

# Perform statistical tests for each variable
data <- read.csv("kproto_complete_data.csv")
skim(data)

################################################################################
# Looking at sig of demographics and offers
################################################################################

# Filtering out other
data1 <- data |>
  filter(gender!= 'O')
# looking at significance of gender on offers rec
wilcox.test(tot_off_rec ~ gender, data = data1)
wilcox.test(bogo_rec ~ gender, data = data1)
wilcox.test(disc_rec ~ gender, data = data1)
wilcox.test(info_rec ~ gender, data = data1)

# looking at sig of age on offers rec
summary(lm(tot_off_rec ~ age, data = data1))
summary(lm(bogo_rec ~ age, data = data1))
summary(lm(disc_rec ~ age, data = data1))
summary(lm(info_rec ~ age, data = data1))

# Looking at sig of income at offers rec
summary(lm(tot_off_rec ~ income, data = data1))
summary(lm(bogo_rec ~ income, data = data1))
summary(lm(disc_rec ~ income, data = data1))
summary(lm(info_rec ~ income, data = data1))


################################################################################
# Looking at significance of diffrences in means between clusters
################################################################################
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
