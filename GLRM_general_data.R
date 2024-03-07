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

# 7 clusters seems to be best on elbow plot
ggplot(error_df, aes(k, tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks=1:k)+
  xlab("Number of Clusters")+
  ylab("Total Within Sum of Squares")
h2o.shutdown()
# create 3-9 clusters
set.seed(173)

model_kmeans3<-kmeans(x=glrm_table, centers=3, iter.max=300,nstart=25)
model_kmeans4<-kmeans(x=glrm_table, centers=4, iter.max=300,nstart=25)
model_kmeans5<-kmeans(x=glrm_table, centers=5, iter.max=300,nstart=25)
model_kmeans6<-kmeans(x=glrm_table, centers=6, iter.max=300,nstart=25)
model_kmeans7<-kmeans(x=glrm_table, centers=7, iter.max=300,nstart=25)
model_kmeans8<-kmeans(x=glrm_table, centers=8, iter.max=300,nstart=25)
model_kmeans9<-kmeans(x=glrm_table, centers=9, iter.max=300,nstart=25)

cluster_membership3<-model_kmeans3$cluster
cluster_membership4<-model_kmeans4$cluster
cluster_membership5<-model_kmeans5$cluster
cluster_membership6<-model_kmeans6$cluster
cluster_membership7<-model_kmeans7$cluster
cluster_membership8<-model_kmeans8$cluster
cluster_membership9<-model_kmeans9$cluster

# add cluster to table

glrm_table$person_id<-rownames(factor_df)
glrm_table$cluster3<-cluster_membership3
glrm_table$cluster4<-cluster_membership4
glrm_table$cluster5<-cluster_membership5
glrm_table$cluster6<-cluster_membership6
glrm_table$cluster7<-cluster_membership7
glrm_table$cluster8<-cluster_membership8
glrm_table$cluster9<-cluster_membership9

# add
p<-fviz_cluster(model_kmeans3, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

p<-fviz_cluster(model_kmeans4, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

p<-fviz_cluster(model_kmeans5, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

p<-fviz_cluster(model_kmeans6, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

p<-fviz_cluster(model_kmeans7, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

p<-fviz_cluster(model_kmeans8, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

p<-fviz_cluster(model_kmeans9, data= glrm_table[,1:4],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

#join with data_wide
glrm_join<-glrm_table %>% select(person_id, cluster3,cluster4,cluster5,cluster6,cluster7,cluster8,cluster9)

data_wide<-left_join(data_wide, glrm_join, by="person_id")
data_wide

#######################
# investigate cluster 3
#######################
data_wide %>% group_by(cluster3) %>% 
  count()

# income increases per group
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(income), median(income), std=sqrt(var(income)))

ggplot(data=data_wide, aes(cluster3, fill=income_bracket))+
  geom_bar(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(income_bracket))

# group 1 is all male
data_wide %>% group_by(cluster3, gender) %>% 
  count()

ggplot(data_wide, aes(cluster3))+
  geom_bar(aes(fill=gender,y=after_stat(count/sum(count))))

# group mean spend differently
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(tot_amount), median(tot_amount), std=sqrt(var(tot_amount)))

ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster3)))+
  geom_histogram(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(factor(cluster3)))

ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster3)))+
  geom_histogram(aes(y = after_stat(count / sum(count))))

# group mean spend differently
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(ave_amount), median(ave_amount), std=sqrt(var(ave_amount)))

ggplot(data=data_wide, aes(ave_amount, fill=factor(cluster3)))+
  geom_histogram(aes(y = after_stat(count / sum(count))))+
  xlim(c(0,100))

ggplot(data=data_wide, aes(ave_amount, fill=factor(cluster3)))+
  geom_histogram(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(factor(cluster3)))

# no. of trans seem similar
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(tot_trans), median(tot_trans), std=sqrt(var(tot_trans)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster3)))+
  geom_bar(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(factor(cluster3)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster3)))+
  geom_histogram(aes(y = after_stat(count / sum(count))))


# less trans in more trans out for cluster 1
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(tot_trans_in), mean(tot_trans_out),median(tot_trans_in), median(tot_trans_out),stdin=sqrt(var(tot_trans_in)),stdout=var(tot_trans_out))

# averagely younger group1
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(age), median(age), std=sqrt(var(age)))

ggplot(data=data_wide, aes(cluster3, fill=age_group))+
  geom_bar(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(age_group))

# no info? 
data_wide %>% group_by(cluster3) %>% 
  summarise(mean(tenure), median(tenure), std=sqrt(var(tenure)))

ggplot(data=data_wide, aes(tenure, fill=factor(cluster3)))+
  geom_histogram(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(factor(cluster3)))


# group 2 and 3 more likely to use the bogo and discount
ggplot(data=data_wide, aes(web_comp_rate, fill=factor(cluster3)))+
  geom_bar(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(factor(cluster3)))

ggplot(data=data_wide, aes(disc_response_rate, fill=factor(cluster3)))+
  geom_bar(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(factor(cluster3)))

ggplot(data=data_wide, aes(bogo_response_rate, fill=factor(cluster3)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster3)))

data_wide %>% group_by(cluster3) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))


#######################
# investigate cluster 4
#######################
# majority in one cluster
data_wide %>% group_by(cluster4) %>% 
  count()

# income different for 4 groups although 2 and 4 are similar and 1 and 3
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(income), median(income), std=sqrt(var(income)))

ggplot(data=data_wide, aes(cluster4, fill=income_bracket))+
  geom_bar(aes(y = after_stat(count / sum(count))))+
  facet_wrap(vars(income_bracket))

# group 1 and 3 is all male
data_wide %>% group_by(cluster4, gender) %>% 
  count()

ggplot(data_wide, aes(cluster4))+
  geom_bar(aes(fill=gender))

# group mean spend differently for all groups
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(tot_amount), median(tot_amount), std=sqrt(var(tot_amount)))

ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster4)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster4)))

ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster4)))+
  geom_histogram()

# group mean spend differently 1and 3 are similar
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(ave_amount), median(ave_amount), std=sqrt(var(ave_amount)))

ggplot(data=data_wide, aes(ave_amount, fill=factor(cluster4)))+
  geom_histogram()+
  xlim(c(0,100))

ggplot(data=data_wide, aes(ave_amount, fill=factor(cluster4)))+
  geom_histogram()+
  xlim(c(0,150))+
  facet_wrap(vars(factor(cluster4)))

# one group goes more regular
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(tot_trans), median(tot_trans), std=sqrt(var(tot_trans)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster4)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster4)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster4)))+
  geom_histogram()


# more trans in for 2 clusters 
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(tot_trans_in), mean(tot_trans_out),median(tot_trans_in), median(tot_trans_out),stdin=sqrt(var(tot_trans_in)),stdout=var(tot_trans_out))

# averagely younger 2 groups
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(age), median(age), std=sqrt(var(age)))

ggplot(data=data_wide, aes(cluster4, fill=age_group))+
  geom_bar()+
  facet_wrap(vars(age_group))

# tenure the younger groups are split by tenure
data_wide %>% group_by(cluster4) %>% 
  summarise(mean(tenure), median(tenure), std=sqrt(var(tenure)))

ggplot(data=data_wide, aes(tenure, fill=factor(cluster4)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster4)))


# the older groups are more likely to complete the offers
ggplot(data=data_wide, aes(web_comp_rate, fill=factor(cluster4)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster4)))

ggplot(data=data_wide, aes(disc_response_rate, fill=factor(cluster4)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster4)))

ggplot(data=data_wide, aes(bogo_response_rate, fill=factor(cluster4)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster4)))

data_wide %>% group_by(cluster4) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))
