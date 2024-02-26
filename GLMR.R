###################
#factor analysis
##################

library(tidyverse)
library(GGally)
library(data.table)
library(factoextra)
library(cluster)
library(h2o)

# load in data 3 (possibly change)
data_wide<-read.csv("data_wide3.csv")


head(data_wide)
colnames(data_wide)

# make a dataset for factor analysis - possibly remove rfm_scores as already in data, can include variables in future
factor_df<- data_wide %>% 
  select(person_id, gender, age, income, tenure, tot_trans, tot_amount, 
         ave_amount, max_amount,tot_tran_in, tot_tran_out, tot_amount_in, 
         tot_amount_out, offer_view_rate, offer_completion_rate, view_to_completion_rate,
         composite_engagement_score, email_comp_rate, mobile_comp_rate, social_comp_rate,
         promotion_interaction_rate,promotion_conversion_rate, perc_reward_cashed, 
         perc_difficulty_cashed, r_score, f_score, m_score, rfm_score)

ggpairs(factor_df %>% select(age, income, tenure, tot_trans, tot_amount) %>% slice_sample(n=1000))
ggpairs(factor_df %>% select(email_comp_rate, mobile_comp_rate, social_comp_rate, tot_tran_in, tot_tran_out) %>% slice_sample(n=1000))
ggpairs(factor_df %>% select(promotion_conversion_rate, perc_reward_cashed, 
                             perc_difficulty_cashed, r_score, f_score, m_score, rfm_score) %>% slice_sample(n=1000))

################
# transform data and check for skewness
#####################
str(factor_df)
# change to log format
hist(factor_df$tot_trans)
hist(log(factor_df$tot_trans))

factor_df<-factor_df %>% mutate(log_tot_trans=log(tot_trans)) %>% 
  select(-tot_trans)

# keep in current format
hist(factor_df$age)
hist(log(factor_df$age))

# keep in current format
hist(factor_df$tenure)
hist(log(factor_df$tenure))
skew(factor_df$tenure)
kurtosi(factor_df$tenure)

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


# keep in count format
hist(factor_df$tot_tran_in)
hist(log(factor_df$tot_tran_in))
skew(factor_df$tot_tran_in)
kurtosi(factor_df$tot_tran_in)
unique(factor_df$tot_tran_in)

hist(factor_df$tot_tran_out)
hist(log(factor_df$tot_tran_out))
skew(factor_df$tot_tran_out)
kurtosi(factor_df$tot_tran_out)

# change to factors
factor_df$r_score<-as.factor(factor_df$r_score)
factor_df$f_score<-as.factor(factor_df$f_score)
factor_df$m_score<-as.factor(factor_df$m_score)



########
#GLRM
########


library(h2o)


factor_df3<-factor_df
#remove na values
factor_df3<-factor_df3[complete.cases(factor_df3),]
factor_df3$gender<- as.factor(factor_df3$gender)

# investigate factor_df3
rownames(factor_df3)<-factor_df3$person_id
factor_df3<-factor_df3 %>% select(-person_id)
head(factor_df3)
sapply(factor_df3, class)

write.csv(factor_df3, file="factor_df.csv")

# initiate h2o
h2o.init()

factor1<-as.h2o(factor_df3)
factor1
class(factor1)

factor2 <- h2o.importFile("factor_df.csv")
factor2

class(factor2)

# change to factor - unsure if it loses factor when converted, unsure if need to do anything with count data
factor1$gender<-h2o::as.factor(factor1$gender)
factor1$r_score<-h2o::as.factor(factor1$r_score)
factor1$f_score<-h2o::as.factor(factor1$f_score)
factor1$m_score<-h2o::as.factor(factor1$m_score)
factor1$rfm_score<-h2o::as.factor(factor1$rfm_score)
factor1

# create train and test data. It is difficult to validate as no. of obs have to be the same in train and valid data. This probabaly needs to be split before changing to h2o format
train <- h2o::h2o.splitFrame(data=factor1, ratios=0.8, seed=123)[[1]]
#valid <- h2o::h2o.splitFrame(data=factor1, ratios=c(0.6,0.2), seed=123)[[2]]
test <- h2o::h2o.splitFrame(data=factor1, ratios=0.8, seed=123)[[2]]
train
test

# perform glrm with all variables in model
glrm_model1 <- h2o.glrm(training_frame=train,
                        seed=123,
                        k=27,
                        loss="Quadratic",
                        regularization_x = "None",
                        regularization_y = "None",
                        transform="STANDARDIZE",
                        svd_method="GramSVD",
                        init="SVD"
)

glrm_model1@model$names

# 555 iterations to converge
h2o::summary(glrm_model1)
# missclassification rate
11059/253642 # 0.0436
# sse 2659.077

# 80% of the data is within first 3 pcs and 90% within first 10
attributes(glrm_model1)

glrm_model1@model$importance


data.frame(Archetype=glrm_model1@model$importance |> seq_along(),
           Percentage_Variance_Explained = glrm_model1@model$importance %>% .[2,] |> unlist(),
           Cumulative_Variance_Explained = glrm_model1@model$importance %>% .[3,] |> unlist()
)|>
  gather(metric, variance_explained, -Archetype)|>
  ggplot(aes(Archetype, variance_explained))+
  geom_point(color="blue", size=3)+
  facet_wrap(~ metric, ncol=1, scales="free")

# check the archetypes
glrm_model1@model$archetypes  

cor(glrm_model1@model$archetypes)


varnumber<-ncol(glrm_model1@model$archetypes)
p1<-t(glrm_model1@model$archetypes) |>
  as.data.frame() %>% 
  mutate(feature=row.names(.))|>
  ggplot(aes(Arch1, reorder(feature, Arch1)))+
  geom_point(color=c(1:varnumber), size=5)+
  labs(title="VariableLoadings for Arcitype 1",x="Variable Loadings on Architype 1", y="Variables")
p1


p2<-t(glrm_model1@model$archetypes) |>
  as.data.frame() %>% 
  mutate(feature=row.names(.))|>
  ggplot(aes(Arch1,Arch2, label=feature))+
  geom_text(colour="blue", size=6, vjust=1.0)+
  geom_point(colour="black", size=3)+
  labs(x="Architype 1", y="Architype2",
       title="Customers in Feature Space")
p2


factor_perf <- h2o.performance(glrm_model1)
factor_perf

factor_pred<- predict(glrm_model1, newdata=test)
factor_pred
factor_pred[2]
head(scale(test),6)
scale(test)[2]
c(factor_pred[1],scale(test)[1])
c(factor_pred[2],scale(test)[2])
c(factor_pred[3], scale(test)[3])
c(factor_pred[4], scale(test)[4])






# train <- h2o::h2o.splitFrame(data=factor1, ratios=c(5600/14373,5600/14373,(14373-5600*2)/14373), seed=123)[[1]]
# valid <- h2o::h2o.splitFrame(data=factor1, ratios=c(5600/14373,5600/14373,(14373-5600*2)/14373)), seed=123)[[2]]
# test <- h2o::h2o.splitFrame(data=factor1, ratios=c(5600/14373,5600/14373,(14373-5600*2)/14373)), seed=123)[[3]]
# train
# test
# valid

# glrm_model2 <- h2o.glrm(training_frame=train,
#                         validation_frame = valid,
#                         seed=123,
#                         k=27,
#                         loss="Quadratic",
#                         regularization_x = "None",
#                         regularization_y = "None",
#                         transform="STANDARDIZE",
#                         svd_method="GramSVD",
#                         init="SVD"
# )


#########
# Clustering using GLRM
#########
head(factor_df3)
factor_df3$id<-seq.int(nrow(factor_df3))
factor_df3$cluster<-1
dim(factor_df3)

factor1<-as.h2o(factor_df3)
# model allcolumns and choose to keep 10 arcitypes(PCs more or less) in dataframe
model_all<-h2o.glrm(training_frame = factor1,
                    seed=123,
                    cols=1:27, 
                    k=10,
                    loss="Quadratic",
                    regularization_x = "None",
                    regularization_y = "None",
                    transform="STANDARDIZE",
                    svd_method="GramSVD",
                    init="SVD",
                    recover_svd = FALSE)

# table the data
glrm_table<-as.data.table(h2o.getFrame(model_all@model$representation_name))
glrm_table

# scree plot
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

# perform k means with 10 clusters
set.seed(123)
model_kmeans<-kmeans(glrm_table[,1:27], centers=10, nstart=25)

cluster_membership<-model_kmeans$cluster
cluster_membership

# add cluster group and person_id (i believe order has been kept)
glrm_table$id<-seq.int(nrow(factor_df3))
glrm_table$cluster<-cluster_membership
glrm_table$person_id<-rownames(factor_df3)

# cluster plot
p<-fviz_cluster(model_kmeans, data= glrm_table[,1:27],
             geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"

# create cluster table to merge with all data
clusters<-glrm_table[,c("cluster", "person_id")]

# merge data
data_wide<-left_join(data_wide, clusters, by="person_id")

##########
# investigate clusters
##########
data_wide %>% group_by(cluster) %>% 
  count()

data_wide %>% group_by(cluster) %>% 
  summarise(mean(income), median(income), std=sqrt(var(income)))

ggplot(data=data_wide, aes(cluster, fill=income_bracket))+
  geom_bar()+
  facet_wrap(vars(income_bracket))

data_wide %>% group_by(cluster, gender) %>% 
  count()

# group 1,6,7,9 male, group 4 female
ggplot(data_wide, aes(cluster))+
  geom_bar(aes(fill=gender))

# group 3 large spenders, group 2,6,7 smaller spenders
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tot_amount), median(tot_amount), std=sqrt(var(tot_amount)))

ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster)))

# GROUP UP 1,2,6,7 AND GROUP UP 5,8 AND 10 (4 AND 9 POSSIBLY IN BETWEEN)
ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster)))+
  geom_histogram()+
  xlim(c(0,500))+
  facet_wrap(vars(factor(cluster)))

data_wide %>% group_by(cluster) %>% 
  summarise(mean(ave_amount), median(ave_amount), std=sqrt(var(ave_amount)))

# group 1 most transactions, group 2 least transactions
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tot_trans), median(tot_trans), std=sqrt(var(tot_trans)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster)))

# no value? wrong data possibly
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tot_tran_in), mean(tot_tran_in),median(tot_tran_in), median(tot_tran_out),stdin=sqrt(var(tot_tran_in)),stdout=var(tot_tran_out),)

# age doesn't appear to be very different
data_wide %>% group_by(cluster) %>% 
  summarise(mean(age), median(age), std=sqrt(var(age)))

ggplot(data=data_wide, aes(cluster, fill=age_group))+
  geom_bar()+
  facet_wrap(vars(age_group))

# tenure
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tenure), median(tenure), std=sqrt(var(tenure)))

ggplot(data=data_wide, aes(tenure, fill=factor(cluster)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster)))


# group 6 worst and group 10 best
data_wide %>% group_by(cluster) %>% 
  summarise(mean(email_comp_rate), median(email_comp_rate), std=sqrt(var(email_comp_rate)))

ggplot(data=data_wide, aes(email_comp_rate, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))


# group 6 worst and group 10 best
data_wide %>% group_by(cluster) %>% 
  summarise(mean(social_comp_rate), median(social_comp_rate), std=sqrt(var(social_comp_rate)))

ggplot(data=data_wide, aes(social_comp_rate, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))


data_wide %>% group_by(cluster) %>% 
  summarise(mean(r_score), median(r_score), std=sqrt(var(r_score)))

ggplot(data=data_wide, aes(r_score, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))


data_wide %>% group_by(cluster) %>% 
  summarise(mean(f_score), median(f_score), std=sqrt(var(f_score)))

ggplot(data=data_wide, aes(f_score, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))

data_wide %>% group_by(cluster) %>% 
  summarise(mean(m_score), median(m_score), std=sqrt(var(m_score)))

ggplot(data=data_wide, aes(m_score, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))


#############################################
# cluster using glrm without the  r_score, etc.
###########################################
# remove from r,f,m score from dataset
factor_df4<-factor_df3 %>% select(-r_score,-m_score, -f_score, -rfm_score)

h2o.init()

factor1<-as.h2o(factor_df4)
factor1
class(factor1)

factor2 <- h2o.importFile("factor_df.csv")
factor2

class(factor2)

# change to factor - unsure if it loses factor when converted, unsure if need to do anything with count data
factor1$gender<-h2o::as.factor(factor1$gender)

factor1

# create train and test data. It is difficult to validate as no. of obs have to be the same in train and valid data. This probabaly needs to be split before changing to h2o format
train <- h2o::h2o.splitFrame(data=factor1, ratios=0.8, seed=123)[[1]]
#valid <- h2o::h2o.splitFrame(data=factor1, ratios=c(0.6,0.2), seed=123)[[2]]
test <- h2o::h2o.splitFrame(data=factor1, ratios=0.8, seed=123)[[2]]
train
test

# perform glrm with all variables in model
glrm_model1 <- h2o.glrm(training_frame=train,
                        seed=123,
                        cols = 1:23,
                        k=23,
                        loss="Quadratic",
                        regularization_x = "None",
                        regularization_y = "None",
                        transform="STANDARDIZE",
                        svd_method="GramSVD",
                        init="SVD"
)

glrm_model1@model$names

# 555 iterations to converge
h2o::summary(glrm_model1)
# missclassification rate


# 80% of the data is within first 6 pcs and 90% within first 8
attributes(glrm_model1)

glrm_model1@model$importance


data.frame(Archetype=glrm_model1@model$importance |> seq_along(),
           Percentage_Variance_Explained = glrm_model1@model$importance %>% .[2,] |> unlist(),
           Cumulative_Variance_Explained = glrm_model1@model$importance %>% .[3,] |> unlist()
)|>
  gather(metric, variance_explained, -Archetype)|>
  ggplot(aes(Archetype, variance_explained))+
  geom_point(color="blue", size=3)+
  facet_wrap(~ metric, ncol=1, scales="free")

# check the archetypes
glrm_model1@model$archetypes  

cor(glrm_model1@model$archetypes)


varnumber<-ncol(glrm_model1@model$archetypes)
p1<-t(glrm_model1@model$archetypes) |>
  as.data.frame() %>% 
  mutate(feature=row.names(.))|>
  ggplot(aes(Arch1, reorder(feature, Arch1)))+
  geom_point(color=c(1:varnumber), size=5)+
  labs(title="VariableLoadings for Arcitype 1",x="Variable Loadings on Architype 1", y="Variables")
p1


p2<-t(glrm_model1@model$archetypes) |>
  as.data.frame() %>% 
  mutate(feature=row.names(.))|>
  ggplot(aes(Arch1,Arch2, label=feature))+
  geom_text(colour="blue", size=6, vjust=1.0)+
  geom_point(colour="black", size=3)+
  labs(x="Architype 1", y="Architype2",
       title="Customers in Feature Space")
p2


factor_perf <- h2o.performance(glrm_model1)
factor_perf

factor_pred<- predict(glrm_model1, newdata=test)
factor_pred
factor_pred[2]
head(scale(test),6)
scale(test)[2]
c(factor_pred[1],scale(test)[1])
c(factor_pred[2],scale(test)[2])
c(factor_pred[3], scale(test)[3])
c(factor_pred[4], scale(test)[4])

head(factor_df3)
factor_df4$id<-seq.int(nrow(factor_df4))
factor_df4$cluster<-1
dim(factor_df4)

factor1<-as.h2o(factor_df4)

model_all<-h2o.glrm(training_frame = factor1,
                    seed=123,
                    cols=1:23, 
                    k=6,
                    loss="Quadratic",
                    regularization_x = "None",
                    regularization_y = "None",
                    transform="STANDARDIZE",
                    svd_method="GramSVD",
                    init="SVD",
                    recover_svd = FALSE)

glrm_table<-as.data.table(h2o.getFrame(model_all@model$representation_name))
glrm_table
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

set.seed(123)
model_kmeans<-kmeans(glrm_table, centers=7, iter.max=100,nstart=25)

cluster_membership<-model_kmeans$cluster
cluster_membership

glrm_table$id<-seq.int(nrow(factor_df4))
glrm_table$cluster<-cluster_membership
glrm_table$person_id<-rownames(factor_df4)

p<-fviz_cluster(model_kmeans, data= glrm_table[,1:6],
                geom=c("point","text"), main="Cluster Plot on First Two Architypes")
p$labels$x<-"Architype 1"
p$labels$y<-"Architype 2"
p

clusters<-glrm_table[,c("cluster", "person_id")]

data_wide<-left_join(data_wide, clusters, by="person_id")


data_wide %>% group_by(cluster) %>% 
  count()

#group 2 and 7 high earners 1,3,4 lower earners
data_wide %>% group_by(cluster) %>% 
  summarise(mean(income), median(income), std=sqrt(var(income)))

ggplot(data=data_wide, aes(cluster, fill=income_bracket))+
  geom_bar()+
  facet_wrap(vars(income_bracket))

data_wide %>% group_by(cluster, gender) %>% 
  count()

# group 1,3 male, group 2,4 mostly female
ggplot(data_wide, aes(cluster))+
  geom_bar(aes(fill=gender))

# group 5 large spenders, group 1 then 4 smaller spenders
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tot_amount), median(tot_amount), std=sqrt(var(tot_amount)))

ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster)))

# GROUP UP 2,3,6 AND GROUP UP 1,4,and 7
ggplot(data=data_wide, aes(tot_amount, fill=factor(cluster)))+
  geom_histogram()+
  xlim(c(0,500))+
  facet_wrap(vars(factor(cluster)))

data_wide %>% group_by(cluster) %>% 
  summarise(mean(ave_amount), median(ave_amount), std=sqrt(var(ave_amount)))

ggplot(data=data_wide, aes(ave_amount, fill=factor(cluster)))+
  geom_histogram()+
  xlim(c(0,25))

ggplot(data=data_wide, aes(ave_amount, fill=factor(cluster)))+
  geom_histogram()+
  xlim(c(0,25))+
  facet_wrap(vars(factor(cluster)))

# group 3 most transactions, group 3 least transactions
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tot_trans), median(tot_trans), std=sqrt(var(tot_trans)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))

ggplot(data=data_wide, aes(tot_trans, fill=factor(cluster)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster)))

# 5 and 6 most affected by offers
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tot_tran_in), mean(tot_tran_out),median(tot_tran_in), median(tot_tran_out),stdin=sqrt(var(tot_tran_in)),stdout=var(tot_tran_out),)

# age group 6 and 7 older
data_wide %>% group_by(cluster) %>% 
  summarise(mean(age), median(age), std=sqrt(var(age)))

ggplot(data=data_wide, aes(cluster, fill=age_group))+
  geom_bar()+
  facet_wrap(vars(age_group))

# tenure 6 and 7 shortest tenure 6 longest
data_wide %>% group_by(cluster) %>% 
  summarise(mean(tenure), median(tenure), std=sqrt(var(tenure)))

ggplot(data=data_wide, aes(tenure, fill=factor(cluster)))+
  geom_histogram()+
  facet_wrap(vars(factor(cluster)))


# group 6most active and group 1  least active
data_wide %>% group_by(cluster) %>% 
  summarise(mean(email_comp_rate), median(email_comp_rate), std=sqrt(var(email_comp_rate)))

ggplot(data=data_wide, aes(email_comp_rate, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))



data_wide %>% group_by(cluster) %>% 
  summarise(mean(social_comp_rate), median(social_comp_rate), std=sqrt(var(social_comp_rate)))

ggplot(data=data_wide, aes(social_comp_rate, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))


data_wide %>% group_by(cluster) %>% 
  summarise(mean(r_score), median(r_score), std=sqrt(var(r_score)))

ggplot(data=data_wide, aes(r_score, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))


data_wide %>% group_by(cluster) %>% 
  summarise(mean(f_score), median(f_score), std=sqrt(var(f_score)))

ggplot(data=data_wide, aes(f_score, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))

data_wide %>% group_by(cluster) %>% 
  summarise(mean(m_score), median(m_score), std=sqrt(var(m_score)))

ggplot(data=data_wide, aes(m_score, fill=factor(cluster)))+
  geom_bar()+
  facet_wrap(vars(factor(cluster)))
