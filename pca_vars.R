library(tidyverse)
library(magrittr)
library(skimr)
library(ggpubr)
library(gridExtra)
library(missMDA)
library(factoextra)



data <- read.csv('data_wide5.csv')
skim(data)

# Selecting variables
sel_vars <- c(
  'age', 'log_income', 'log_tenure', 'log_tot_trans',
  'log_tot_amount', 'log_ave_amount', 'log_max_amount',
  'log_ave_amount_in', 'log_ave_amount_out',
  'tot_reward_rec', 'reward_rec_rate',
  'offer_view_rate', 'offer_completion_rate',
  'bogo_view_rate', 'bogo_response_rate',
  'disc_view_rate', 'disc_response_rate',
  'web_view_rate','web_comp_rate',
  'mob_view_rate','mob_comp_rate',
  'social_view_rate', 'social_comp_rate'
  )
# NOTE: I did not include info_view rate as its missing too many observations
pca_vars <- data[,sel_vars]

# Some variables have NAs
skim(pca_vars)
missing <- pca_vars[!complete.cases(pca_vars),]

################################################################################
# Using missMDA to imput missing values
################################################################################

# centering and scaling data
pca_vars <- scale(pca_vars, center = TRUE, scale = TRUE)
# estimating the number of components to use during missing value imputation
nd <- estim_ncpPCA(pca_vars)
# Imputing missing values
pca_vars_comp <- imputePCA(pca_vars, ncp = nd$ncp)
# conducting PCA with complete data

################################################################################
# PCA with completed data set
################################################################################

pca <- prcomp(pca_vars_comp$completeObs)
summary(pca)
# scree plot
fviz_eig(pca, addlabels = TRUE) # four components possible is best?
#Variable importance plot using first 2 components
fviz_cos2(pca, choice = "var", axes = 1:2)

write.csv(pca$x, 'pca_components.csv', row.names = FALSE)

