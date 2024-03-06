library(tidyverse)

transcript<-read.csv("transcript.csv")
portfolio<-read.csv("portfolio.csv")
profile<-read.csv("profile.csv")

colnames(transcript)

offer_rec_time<-transcript %>% filter(offer_received==1) %>% 
  group_by(time)  %>% 
  count() # 0,168,336,408,504,576

# group offers by period
period1<- transcript %>% filter(time<168)
period2<- transcript %>% filter(time>=168, time<336 )
period3<- transcript %>% filter(time>=336, time<408)
period4<- transcript %>% filter(time>=408, time<504)
period5<- transcript %>% filter(time>=504, time<576)
period6<- transcript %>% filter(time>=576)

# check when people view an offer
view_period1<-transcript %>% filter(offer_viewed==1, time<168) %>% 
  group_by(time) %>% 
  count()
view_period1$time
view_period1$n

view_period2<-transcript %>% filter(offer_viewed==1, time>=168, time<336) %>% 
  group_by(time) %>% 
  count()
view_period2$time
view_period2$n 

view_period6<-transcript %>% filter(offer_viewed==1, time>=576) %>% 
  group_by(time) %>% 
  count()
view_period6$time
view_period6$n 



period6 %>% filter(offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period5 %>% filter( offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period4 %>% filter(offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period3 %>% filter(offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period2 %>% filter(offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period1 %>% filter(offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

# someone completes 4 offers in period 6
period6 %>% filter(offer_completed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period5 %>% filter(offer_completed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period4 %>% filter(offer_completed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period3 %>% filter(offer_completed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period2 %>% filter(offer_completed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

period1 %>% filter(offer_completed==1) %>% 
  group_by(person_id) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  distinct()

##############################################################
# make dataset of time to view offers 

time_view_first_offer<-period1 %>% filter(offer_viewed==1) %>% 
  select(person_id,time_to_view_first_offer=time)
time_view_second_offer<-period2 %>% filter(offer_viewed==1) %>% 
  mutate(time_view_second_offer = (time-168)) %>% 
  select(person_id,time_view_second_offer)  
time_view_third_offer<-period3 %>% filter(offer_viewed==1) %>% 
  mutate(time_view_third_offer = (time-336)) %>% 
  select(person_id,time_view_third_offer)  
time_view_forth_offer<-period4 %>% filter(offer_viewed==1) %>% 
  mutate(time_view_forth_offer = (time-408)) %>% 
  select(person_id,time_view_forth_offer) 
time_view_fifth_offer<-period5 %>% filter(offer_viewed==1) %>% 
  mutate(time_view_fifth_offer = (time-504)) %>% 
  select(person_id,time_view_fifth_offer) 
time_view_sixth_offer<-period6 %>% filter(offer_viewed==1) %>% 
  mutate(time_view_sixth_offer = (time-576)) %>% 
  select(person_id,time_view_sixth_offer) 

# join all offers received for each period
offer_view_times<-full_join(time_view_first_offer,time_view_second_offer, by="person_id")
offer_view_times<-full_join(offer_view_times,time_view_third_offer, by="person_id")
offer_view_times<-full_join(offer_view_times,time_view_forth_offer, by="person_id")
offer_view_times<-full_join(offer_view_times,time_view_fifth_offer, by="person_id")
offer_view_times<-full_join(offer_view_times,time_view_sixth_offer, by="person_id")

head(offer_view_times)
summary(offer_view_times)

# create total view count and average view count table
offer_view_times["tot_view_count"]<-rowSums(offer_view_times[,2:7], na.rm = TRUE)
offer_view_times["ave_view_count"]<-rowSums(offer_view_times[,2:7], na.rm = TRUE)/rowSums(is.na(offer_view_times[,2:7])==FALSE)
dim(offer_view_times)

# read in wide dataset and left join with offer view times
data_wide<-read.csv("data_wide_temp.csv")
dim(data_wide)
offer_view_times
data_wide <- left_join(data_wide, offer_view_times, by="person_id")
dim(data_wide)

# check patterns in data
ggplot(data_wide,aes(ave_view_count))+
  geom_histogram()

ggplot(data_wide,aes(log(ave_view_count)))+
  geom_histogram()

ggplot(data_wide,aes(log(ave_view_count)))+
  geom_histogram()+
  facet_wrap(vars(gender))

ggplot(data_wide,aes(log(ave_view_count)))+
  geom_histogram()+
  facet_wrap(vars(m_score))

ggplot(data_wide,aes(ave_view_count))+
  geom_histogram()+
  facet_wrap(vars(f_score))

ggplot(data_wide,aes(ave_view_count))+
  geom_histogram()+
  facet_wrap(vars(age_group))

ggplot(data_wide,aes(ave_view_count, age))+
  geom_point()

ggplot(data_wide, aes(y=ave_view_count))+
  geom_boxplot(aes(fill=factor(m_score)))
  
ggplot(data_wide, aes(y=ave_view_count))+
  geom_boxplot(aes(fill=factor(f_score)))

ggplot(data_wide, aes(y=ave_view_count))+
  geom_boxplot(aes(fill=factor(r_score)))

ggplot(data_wide, aes(y=ave_view_count))+
  geom_boxplot(aes(fill=factor(age_group)))

ggplot(data_wide, aes(y=ave_view_count))+
  geom_boxplot(aes(fill=factor(income_bracket)))

ggplot(data_wide, aes(y=ave_view_count))+
  geom_boxplot(aes(fill=factor(gender)))

summary(data_wide$ave_view_count)
summary(data_wide$total_view_count)


unique(period2$time)
unique(time_view_second_offer$time_view_second_offer)
max(time_view_second_offer$time_view_second_offer)
transcript %>% filter(offer_viewed ==1 & time==0)

period1








# some analysis - donot read as messy - I've kept if we want to do further analysis

rec_and_view_period1<-period1 %>% filter(offer_received==1 | offer_viewed==1) %>% 
  group_by(person_id) %>% 
  count() 

rec_and_view_period1["rec_and_view_offer"]<-ifelse(rec_and_view_period1$n==2, 1,0)  

rec_and_view_period1<-rec_and_view_period1 %>% select(person_id, rec_and_view_offer)

period1<-left_join(period1,rec_and_view_period1, by="person_id")
period1<-left_join(period1,time_view_first_offer, by="person_id")
period1["rec_offer_period1"]<-ifelse(is.na(period1$rec_and_view_offer),0,1)
summary(period1$rec_offer_period1)

summary(period1$rec_and_view_offer)
summary(period1$rec_and_view_offer)
period1$rec_offer_period1


max(time_view_first_offer$time_to_view_first_offer)
max(period1$time)

period1<-period1 %>% group_by(person_id) %>% 
  mutate(after_offer_view = ifelse(time>=time_to_view_first_offer & time<time_to_view_first_offer +72,1,0))

ave_spend_after_view_offer<-period1 %>% 
  filter(after_offer_view==1) %>% 
  group_by(person_id) %>% 
  summarise(ave_spend_after_view_offer=sum(amount, na.rm=TRUE)/(72))

ave_spend_before_view_offer  <-period1 %>% 
  filter(after_offer_view==0) %>% 
  group_by(person_id) %>% 
  summarise(ave_spend_before_view_offer=sum(amount, na.rm=TRUE)/(162-72))

before_after<-merge(ave_spend_after_view_offer,ave_spend_before_view_offer, by="person_id")

ggplot(data=before_after %>% sample_n(500), aes(ave_spend_after_view_offer, ave_spend_before_view_offer))+
  geom_point()+
  geom_abline(slope=1, intercept=0)+
  xlim(c(0,2))+
  ylim(c(0,2))


transcript
