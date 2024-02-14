library(tidyverse)
colnames(data_wide)


data_wide %>% group_by(trans_offer1) %>%
      count()

data_wide %>% group_by(trans_offer2) %>%
      count()

#summary stats on amount offer 1-10

summary(data_wide %>% dplyr::select(amount_offer1,amount_offer2,
                     amount_offer3,amount_offer4,
                     amount_offer5,amount_offer6,
                     amount_offer7,amount_offer8,
                     amount_offer9,amount_offer10))

#summary stats on trans offer 1-10
summary(data_wide %>% dplyr::select(trans_offer1,trans_offer2,
                             trans_offer3,trans_offer4,
                             trans_offer5,trans_offer6,
                             trans_offer7,trans_offer8,
                             trans_offer9,trans_offer10))      



# income and  age factor included in data
data_wide2<-data_wide %>% 
      mutate(income_factor=cut(income,breaks=c(29999,39999,49999,59999,69999, 79999,89999,120000)),
             age_factor=cut(age, breaks=c(17, 29, 39, 49, 59, 69, 101)))      

# check if mean changes with trans during offers 
age_fact_trans<-data_wide2 %>% 
      dplyr::select(trans_offer1,trans_offer2,
             trans_offer3,trans_offer4,
             trans_offer5,trans_offer6,
             trans_offer7,trans_offer8,
             trans_offer9,trans_offer10, 
             age_factor, income_factor) %>% 
      group_by(age_factor) %>% 
      summarise(mean_trans1=mean(trans_offer1, na.rm=TRUE),mean_trans2=mean(trans_offer2, na.rm=TRUE),
                mean_trans3=mean(trans_offer3, na.rm=TRUE),mean_trans4=mean(trans_offer4, na.rm=TRUE),
                mean_trans5=mean(trans_offer5, na.rm=TRUE),mean_trans6=mean(trans_offer6, na.rm=TRUE),
                mean_trans7=mean(trans_offer7, na.rm=TRUE),mean_trans8=mean(trans_offer8, na.rm=TRUE),
                mean_trans9=mean(trans_offer9, na.rm=TRUE),mean_trans10=mean(trans_offer10, na.rm=TRUE))
age_fact_trans

age_fact_amount<-data_wide2 %>% 
      dplyr::select(amount_offer1,amount_offer2,
             amount_offer3,amount_offer4,
             amount_offer5,amount_offer6,
             amount_offer7,amount_offer8,
             amount_offer9,amount_offer10, 
             age_factor, income_factor) %>% 
      group_by(age_factor) %>% 
      summarise(mean_amount1=mean(amount_offer1, na.rm=TRUE),mean_amount2=mean(amount_offer2, na.rm=TRUE),
                mean_amount3=mean(amount_offer3, na.rm=TRUE),mean_amount4=mean(amount_offer4, na.rm=TRUE),
                mean_amount5=mean(amount_offer5, na.rm=TRUE),mean_amount6=mean(amount_offer6, na.rm=TRUE),
                mean_amount7=mean(amount_offer7, na.rm=TRUE),mean_amount8=mean(amount_offer8, na.rm=TRUE),
                mean_amount9=mean(amount_offer9, na.rm=TRUE),mean_amount10=mean(amount_offer10, na.rm=TRUE))
age_fact_amount

income_fact_amount<-data_wide2 %>% 
      dplyr::select(amount_offer1,amount_offer2,
             amount_offer3,amount_offer4,
             amount_offer5,amount_offer6,
             amount_offer7,amount_offer8,
             amount_offer9,amount_offer10, 
             age_factor, income_factor) %>% 
      group_by(income_factor) %>% 
      summarise(mean_amount1=mean(amount_offer1, na.rm=TRUE),mean_amount2=mean(amount_offer2, na.rm=TRUE),
                mean_amount3=mean(amount_offer3, na.rm=TRUE),mean_amount4=mean(amount_offer4, na.rm=TRUE),
                mean_amount5=mean(amount_offer5, na.rm=TRUE),mean_amount6=mean(amount_offer6, na.rm=TRUE),
                mean_amount7=mean(amount_offer7, na.rm=TRUE),mean_amount8=mean(amount_offer8, na.rm=TRUE),
                mean_amount9=mean(amount_offer9, na.rm=TRUE),mean_amount10=mean(amount_offer10, na.rm=TRUE))
income_fact_amount


data_wide %>% group_by(tot_off_rec) %>% 
  summarise(min(tenure), max(tenure))

transcript<-read.csv("transcript.csv")
# check transactions for people with no offers
data_wide %>% filter(tot_off_rec==0) %>% 
  dplyr::select(person_id)

transcript %>% dplyr::filter(person_id %in% c("c6e579c6821c41d1a7a6a9cf936e91bb","eb540099db834cf59001f83a4561aef3",
                                              "3a4874d8f0ef42b9a1b72294902afea9","ae8111e7e8cd4b60a8d35c42c1110555",
                                              "12ede229379747bd8d74ccdc20097ca3")) %>% 
  summarise(min(time),max(time))


transcript %>% dplyr::filter(person_id %in% c("eb540099db834cf59001f83a4561aef3")) %>% 
  summarise(min(time),max(time))

# look at min tenure and if transactions fall in a smaller period of time
min_tenure<-data_wide %>% filter(tenure==min(tenure)) %>% dplyr::select(person_id)


transcript %>% filter(person_id %in% min_tenure$person_id & transaction==1) %>% 
  summarise(min(time),max(time))

# look at max tenure and if transactions fall in a smaller period of time
data_wide %>% filter(tenure==max(tenure)) %>% dplyr::select(person_id)
transcript %>% filter(person_id %in% c("77388a70eaf14433b4efc5d01fa947bf") & transaction==1) %>% 
  summarise(min(time),max(time))
