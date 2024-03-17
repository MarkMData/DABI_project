library(tidyverse)
library(magrittr)
library(GGally)


################################################################################
# loading data and preliminary wrangling
################################################################################
data_wide<-read.csv("data_wide_temp.csv")
transcript <- read.csv('transcript.csv')
portfolio <- read.csv('portfolio.csv')
profile <- read.csv('profile.csv')
# dropping the X columns in the csv files
transcript$X <- NULL
portfolio$X <- NULL
profile$X <- NULL


# creating tenure variable using membership start date
profile$tenure <- as.integer(
  difftime(max(profile$membership_start),
           profile$membership_start, unit = 'days')+1
)

# Changing the column names for id and reward
colnames(profile)[colnames(profile) == 'id'] <- 'person_id'
colnames(portfolio)[colnames(portfolio) == 'id'] <- 'offer_id'
colnames(portfolio)[colnames(portfolio) == 'reward'] <- 'reward_off'
colnames(transcript)[colnames(transcript) == 'reward'] <- 'reward_rec'


# Adding duration and offer number to transcript to be used in matching offers
# and transactions


# Creating an additional column for the portfolio df to indicate offer number as
# The offer id is hard to use

portfolio$offer_num <- c('offer1', 'offer2', 'offer3', 'offer4', 'offer5',
                         'offer6', 'offer7','offer8', 'offer9', 'offer10')

# changing the offer duration to hours to match the transcript df
portfolio$duration <- portfolio$duration*24
portfolio

write.csv(portfolio, "portfolio2.csv")

# Changing the NA values in reward and amount to zero
transcript$reward_rec[is.na(transcript$reward_rec)] <- 0
transcript$amount[is.na(transcript$amount)] <- 0

# Joining some of the columns from portfolio to transcript to help with matching 
# transactions and offers
transcript2 <- left_join(
  transcript, portfolio[,c('offer_id','duration', 'offer_num')], by = 'offer_id')
transcript2 %>% filter(person_id=="0020c2b971eb4e9188eac86d93036a77")

write.csv(transcript2, "transcript2.csv", row.names = FALSE)

# Splitting DF into offer periods
period1 <- transcript2 |> filter(time < 168)
period2 <- transcript2 |> filter(time >= 168 & time < 336)
period3 <- transcript2 |> filter(time >= 336 & time < 408)
period4 <- transcript2 |> filter(time >= 408 & time < 504)
period5 <- transcript2 |> filter(time >= 504 & time < 576)
period6 <- transcript2 |> filter(time >= 576)


trans_per_cust <- function(X){
  X |>
    group_by(person_id) |>
    summarise(num_trans = sum(transaction),
              tot_amount = sum(amount, na.rm=TRUE),
              ave_amount = mean(amount, na.rm=TRUE))
    
}

offer_type<-function(X){
  for(i in 1:dim(X)[1]){
    if(X$offer_num[i] %in% c("offer1", "offer2", "offer4", "offer9")){
      X$offer_type[i]<-"bogo"
    }
    if(period1$offer_num[i] %in% c("offer5", "offer6", "offer7", "offer10")){
      X$offer_type[i]<-"discount"
    }
    if(period1$offer_num[i] %in% c("offer3", "offer8")){
      X$offer_type[i]<-"informational"
    }
  }
  return(X)
}



p1_trans<-trans_per_cust(period1) %>% rename(num_trans1=num_trans,tot_amount1=tot_amount, ave_amount1=ave_amount)
p2_trans<-trans_per_cust(period2) %>% rename(num_trans2=num_trans,tot_amount2=tot_amount, ave_amount2=ave_amount)
p3_trans<-trans_per_cust(period3) %>% rename(num_trans3=num_trans,tot_amount3=tot_amount, ave_amount3=ave_amount)
p4_trans<-trans_per_cust(period4) %>% rename(num_trans4=num_trans,tot_amount4=tot_amount, ave_amount4=ave_amount)
p5_trans<-trans_per_cust(period5) %>% rename(num_trans5=num_trans,tot_amount5=tot_amount, ave_amount5=ave_amount)
p6_trans<-trans_per_cust(period6) %>% rename(num_trans6=num_trans,tot_amount6=tot_amount, ave_amount6=ave_amount)

period1<-offer_type(period1)
period2<-offer_type(period2)
period3<-offer_type(period3)
period4<-offer_type(period4)
period5<-offer_type(period5)
period6<-offer_type(period6)

off_rec1<-period1 %>% filter(offer_received==1) %>% select(person_id, offer_type) %>% rename(offer_type1=offer_type)
off_rec2<-period2 %>% filter(offer_received==1) %>% select(person_id, offer_type) %>% rename(offer_type2=offer_type)
off_rec3<-period3 %>% filter(offer_received==1) %>% select(person_id, offer_type) %>% rename(offer_type3=offer_type)
off_rec4<-period4 %>% filter(offer_received==1) %>% select(person_id, offer_type) %>% rename(offer_type4=offer_type)
off_rec5<-period5 %>% filter(offer_received==1) %>% select(person_id, offer_type) %>% rename(offer_type5=offer_type)
off_rec6<-period6 %>% filter(offer_received==1) %>% select(person_id, offer_type) %>% rename(offer_type6=offer_type)


data_wide5 <- left_join(data_wide, off_rec1, by="person_id") 
data_wide5 <- left_join(data_wide5, off_rec2, by="person_id") 
data_wide5 <- left_join(data_wide5, off_rec3, by="person_id") 
data_wide5 <- left_join(data_wide5, off_rec4, by="person_id") 
data_wide5 <- left_join(data_wide5, off_rec5, by="person_id") 
data_wide5 <- left_join(data_wide5, off_rec6, by="person_id") 
data_wide5 <- left_join(data_wide5, p1_trans, by="person_id") 
data_wide5 <- left_join(data_wide5, p2_trans, by="person_id") 
data_wide5 <- left_join(data_wide5, p3_trans, by="person_id") 
data_wide5 <- left_join(data_wide5, p4_trans, by="person_id") 
data_wide5 <- left_join(data_wide5, p5_trans, by="person_id") 
data_wide5 <- left_join(data_wide5, p6_trans, by="person_id") 
data_wide5


dim(data_wide5)
data_wide5[,65:82][is.na(data_wide5[,65:82])] <- 0
data_wide5[,59:64][is.na(data_wide5[,59:64])] <- "none"
colnames(data_wide5)
data_wide5

data_wide5<-data_wide5 %>% select(-view_to_completion_rate)
colnames(data_wide5)

# change log_tot_amount
summary(log(data_wide5$tot_amount+0.1))
ggplot(data=data_wide5, aes(tot_amount))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(tot_amount+0.1)))+
  geom_histogram()

summary(log(data_wide5$ave_amount+0.1))
ggplot(data=data_wide5, aes(ave_amount))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(ave_amount+0.1)))+
  geom_histogram()

summary(log(data_wide5$tot_trans+0.1))
ggplot(data=data_wide5, aes(tot_trans))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(tot_trans+0.1)))+
  geom_histogram()

summary(log(data_wide5$max_amount+0.1))
ggplot(data=data_wide5, aes(max_amount))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(max_amount+0.1)))+
  geom_histogram()

summary(log(data_wide5$ave_amount_in+0.1))
ggplot(data=data_wide5, aes(ave_amount_in))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(ave_amount_in+0.1)))+
  geom_histogram()

summary(log(data_wide5$ave_amount_out+0.1))
ggplot(data=data_wide5, aes(ave_amount_out))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(ave_amount_out+0.1)))+
  geom_histogram()

#DON'T CHANGE
summary(log(data_wide5$tot_trans_in+0.1))
ggplot(data=data_wide5, aes(tot_trans_in))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(tot_trans_in+0.1)))+
  geom_histogram()

#DON'T CHANGE
summary(log(data_wide5$tot_trans_out+0.1))
ggplot(data=data_wide5, aes(tot_trans_out))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(tot_trans_in+0.1)))+
  geom_histogram()

#DON'T CHANGE
summary(log(data_wide5$promotion_interaction_rate+0.1))
ggplot(data=data_wide5, aes(promotion_interaction_rate))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(promotion_interaction_rate+0.1)))+
  geom_histogram()

#DON'T CHANGE
summary(log(data_wide5$tenure))
ggplot(data=data_wide5, aes(tenure))+
  geom_histogram()
ggplot(data=data_wide5, aes(log(tenure)))+
  geom_histogram()

data_wide5<-data_wide5 %>% mutate(log_tot_amount=log(tot_amount+0.1), log_ave_amount=log(ave_amount+0.1),
                      log_tot_trans=log(tot_trans+0.1), log_max_amount=log(max_amount+0.1),
                      log_ave_amount_in=log(ave_amount_in+0.1), log_ave_amount_out=log(ave_amount_out+0.1),
                      log_income=log(income), log_tenure=log(tenure))


write.csv(data_wide5, "data_wide5.csv")

# data_wide5 %>% group_by(cluster3, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1))
# data_wide5 %>% group_by(cluster3, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2))
# data_wide5 %>% group_by(cluster3, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3))
# data_wide5 %>% group_by(cluster3, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4))
# data_wide5 %>% group_by(cluster3, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5))
# data_wide5 %>% group_by(cluster3, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))
