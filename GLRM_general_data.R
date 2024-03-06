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
factor_df<- data_wide %>% select(person_id,gender, age, tenure, tot_trans, tot_amount, ave_amount, max_amount, tot_trans_out, tot_trans_in, reward_rec_rate, income)


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

# set row name to person id
rownames(factor_df)<-factor_df$person_id
factor_df<- factor_df %>% select(-person_id)

########
#GLRM
########

h2o.init()

# create h2o dataframe
all_data <- as.h2o(factor_df)
str(all_data)
all_data
glrm_model_all <- h2o.glrm(training_frame=all_data,
                           seed=123,
                           k=11,
                           loss="Quadratic",
                           regularization_x = "None",
                           regularization_y = "None",
                           transform="STANDARDIZE",
                           svd_method="GramSVD",
                           init="SVD"
)



# predict missing variables #61 missing variables
reconstructed_all_data<-h2o::h2o.reconstruct(glrm_model_all, all_data, reverse_transform = TRUE)
base::names(reconstructed_all_data) = base::names(all_data)

# 80% of data is contained with 4 architypes
glrm_model_all@model$importance

# create glrm with only 4 architypes
glrm_model_4 <- h2o.glrm(training_frame=all_data,
                           seed=123,
                           k=4,
                           loss="Quadratic",
                           regularization_x = "None",
                           regularization_y = "None",
                           transform="STANDARDIZE",
                           svd_method="GramSVD",
                           init="SVD"
)

# predict missing variables #61 missing variables
reconstructed_4_data<-h2o::h2o.reconstruct(glrm_model_4, all_data, reverse_transform = TRUE)
base::names(reconstructed_4_data) = base::names(all_data)

# check importance of architypes
glrm_model_4@model$importance

# crate dataframe of architypes1 to 4 for every customer
glrm_table<-as.data.table(h2o.getFrame(glrm_model_4@model$representation_name))
glrm_table

# create elbow plot
k<-50
tot_withinss<-map_dbl(1:k, function(k){
  model<-kmeans(x=glrm_table, centers=k)
  model$tot.withinss
})
tot_withinss

error_df <- data.frame(k=1:k, tot_withinss=tot_withinss)

ggplot(error_df, aes(k, tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks=1:k)+
  xlab("Number of Clusters")+
  ylab("Total Within Sum of Squares")
