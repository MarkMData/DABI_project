###################
#GLRM
##################

library(tidyverse)
library(GGally)
library(data.table)
library(factoextra)
library(cluster)
library(h2o)
library(psych)

# load in data wide
data_wide<-read.csv("data_wide_temp.csv")
colnames(data_wide)

# create dataframe for glrm with general traits
factor_df<- data_wide %>% select(gender, age, tenure, tot_trans, tot_amount, ave_amount, max_amount, tot_trans_out, tot_trans_in, reward_rec_rate, income)


# create factor variable of gender - doesn't need to be one hot coding for GLRM
factor_df$gender<- as.factor(factor_df$gender)

# keep age in current format
hist(factor_df$age)
hist(log(factor_df$age))

# keep in current format
hist(factor_df$tenure)
hist(log(factor_df$tenure))
skew(factor_df$tenure)
kurtosi(factor_df$tenure)
skew(log(factor_df$tenure))
kurtosi(log(factor_df$tenure))


# keep in same format
hist(factor_df$tot_trans)
hist(log(factor_df$tot_trans))
min(log(factor_df$tot_trans))

# log income
hist(factor_df$income)
hist(log(factor_df$income))
skew(factor_df$income)
kurtosi(factor_df$income)


factor_df<-factor_df %>% mutate(log_income=log(income)) %>% 
  select(-income)

skew(factor_df$log_income)
kurtosi(factor_df$log_income)


# keep in current format as leads to na values
hist(factor_df$tot_amount)
hist(log(factor_df$tot_amount))
skew(factor_df$tot_amount)
kurtosi(factor_df$tot_amount)
skew(log(factor_df$tot_amount))
kurtosi(log(factor_df$tot_amount))


# keep in current format as leads to na values
hist(factor_df$ave_amount)
hist(log(factor_df$ave_amount))
skew(factor_df$ave_amount)
kurtosi(factor_df$ave_amount)
skew(log(factor_df$ave_amount))
kurtosi(log(factor_df$ave_amount))

# keep in count format
hist(factor_df$tot_trans_in)
hist(log(factor_df$tot_trans_in))
skew(factor_df$tot_trans_in)
kurtosi(factor_df$tot_trans_in)
unique(factor_df$tot_trans_in)

# keep in count format
hist(factor_df$tot_trans_out)
hist(log(factor_df$tot_trans_out))
skew(factor_df$tot_trans_out)
kurtosi(factor_df$tot_trans_out)

# keep in count format
hist(factor_df$reward_rec_rate)
hist(log(factor_df$reward_rec_rate))
skew(factor_df$reward_rec_rate)
kurtosi(factor_df$reward_rec_rate)


