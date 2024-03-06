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
factor_df_general<- data_wide %>% select(gender, age, tenure, tot_trans, tot_amount, ave_amount, max_amount, tot_trans_out, tot_trans_in, reward_rec_rate, income)



