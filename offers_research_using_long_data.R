data_long<-read.csv("data_long2.csv")
data_wide<-read.csv("data_wide2.csv")
portfolio<-read.csv("portfolio.csv")
portfolio
e %>% select(reward_off_offer1)

colnames(data_long)
head(data_long)
data_long %>% select(reward, reward_off)
###########################################################
#look how people interact with offers
###########################################################
# check number of offers completed
data_long %>% filter(bogo==1) %>% 
  group_by(offer_num) %>% 
  summarise(sum(off_comp))

# check channels offers were delivered # is error in reward?
data_long %>% filter(bogo==1) %>% 
  group_by(offer_num) %>% 
  mutate(tot_off_comp=sum(off_comp),tot_off_rec= sum(off_rec), tot_off_view=sum(off_view)) %>% 
  select(offer_num, tot_off_rec, tot_off_view, tot_off_comp,reward_off,difficulty, duration, mobile, social, web) %>% 
  slice(1)

data_long %>% filter(discount==1) %>% 
  group_by(offer_num) %>% 
  mutate(tot_off_comp=sum(off_comp),tot_off_rec= sum(off_rec), tot_off_view=sum(off_view)) %>% 
  select(offer_num, tot_off_rec, tot_off_view, tot_off_comp, reward_off,difficulty, duration, mobile, social, web) %>% 
  slice(1)

data_long %>% filter(informational==1) %>% 
  group_by(offer_num) %>% 
  mutate(tot_off_comp=sum(off_comp),tot_off_rec= sum(off_rec), tot_off_view=sum(off_view)) %>% 
  select(offer_num, tot_off_rec, tot_off_view, tot_off_comp, reward_off,difficulty, duration, mobile, social, web) %>% 
  slice(1)
###########################################################

###########################################################
# make new dataframe with offer percentages
##########################################################
# create dataframe with bogo voucher interactions
bogo_df<-data_long %>% 
  filter(off_rec==1, bogo==1) %>% 
  mutate(bogo_difficulty_comp=(difficulty*off_comp),
         bogo_difficulty_view=(difficulty*off_view), 
         bogo_difficulty_rec=(difficulty*off_rec)) %>% 
  group_by(person_id) %>% 
  summarise(tot_bogo_reward_rec=sum(reward_offer),
            tot_bogo_reward_comp=sum(reward),
            tot_bogo_comp=sum(off_comp), 
            tot_bogo_view=sum(off_view),
            tot_bogo_rec=sum(off_rec), 
            tot_amount_bogo=sum(amount), 
            tot_difficulty_bogo_comp=sum(bogo_difficulty_comp),
            tot_difficulty_bogo_view=sum(bogo_difficulty_view),
            tot_difficulty_bogo_rec=sum(bogo_difficulty_rec)) %>% 
  mutate(perc_bogo_reward_comp=tot_bogo_reward_comp/tot_bogo_reward_rec,
         perc_bogo_comp=tot_bogo_comp/tot_bogo_rec,
         perc_bogo_view=tot_bogo_view/tot_bogo_rec,
         perc_bogo_view_comp=tot_bogo_comp/tot_bogo_view, 
         perc_bogo_difficulty_comp=tot_difficulty_bogo_comp/tot_difficulty_bogo_rec,
         perc_bogo_difficulty_view=tot_difficulty_bogo_view/tot_difficulty_bogo_rec,
         perc_bogo_difficulty_view_comp=tot_difficulty_bogo_comp/tot_difficulty_bogo_view)

# quick analysis
colnames(bogo_df)

ggplot(bogo_df, aes(perc_bogo_comp,perc_bogo_difficulty_comp))+
  geom_point()

ggplot(bogo_df, aes(perc_bogo_view,perc_bogo_comp))+
  geom_point()

ggplot(bogo_df, aes(tot_bogo_view,tot_bogo_rec))+
  geom_point()


ggplot(bogo_df, aes(y=tot_bogo_view))+
  geom_boxplot(aes(fill=factor(tot_bogo_comp)))

bogo_df %>% group_by(tot_bogo_rec,tot_bogo_comp) %>% 
  count()

bogo_df %>% group_by(tot_bogo_rec,tot_bogo_view) %>% 
  count()

bogo_df %>% group_by(tot_bogo_view,tot_bogo_comp) %>% 
  count()

bogo_df %>% group_by(tot_bogo_rec) %>% 
  count()

bogo_df %>% group_by(tot_bogo_comp) %>% 
  count()

bogo_df %>% count()

# create a discount dataframe
discount_df<-data_long %>% 
  filter(off_rec==1, discount==1) %>% 
  mutate(discount_difficulty_comp=(difficulty*off_comp), 
         discount_difficulty_view=(difficulty*off_view), 
         discount_difficulty_rec=(difficulty*off_rec)) %>% 
  group_by(person_id) %>% 
  summarise(tot_discount_reward_rec=sum(reward_offer),
            tot_discount_reward_comp=sum(reward),
            tot_discount_comp=sum(off_comp), 
            tot_discount_rec=sum(off_rec), 
            tot_amount_bogo=sum(amount),
            tot_discount_view=sum(off_view),
            tot_difficulty_discount_comp=sum(discount_difficulty_comp),
            tot_difficulty_discount_rec=sum(discount_difficulty_rec),
            tot_difficulty_discount_view=sum(discount_difficulty_view)
            ) %>% 
  mutate(perc_discount_reward_comp=tot_discount_reward_comp/tot_discount_reward_rec,
         perc_discount_comp=tot_discount_comp/tot_discount_rec,
         perc_discount_difficulty_comp=tot_difficulty_discount_comp/tot_difficulty_discount_rec,
         perc_discount_view=tot_discount_view/tot_discount_rec,
         perc_discount_view_comp=tot_discount_comp/tot_discount_view)

# quick analysis of data
ggplot(discount_df, aes(perc_discount_comp,perc_discount_difficulty_comp))+
  geom_point()

discount_df %>% group_by(tot_discount_rec) %>% 
  count()

discount_df %>% group_by(tot_discount_comp) %>% 
  count()

discount_df %>% group_by(tot_discount_rec,tot_discount_comp) %>% 
  count()

discount_df %>% group_by(tot_discount_rec,tot_discount_view) %>% 
  count()

discount_df %>% group_by(tot_discount_view,tot_discount_comp) %>% 
  count()

discount_df %>% count()


# create an inform dataset
inform_df<-data_long %>% 
  filter(off_rec==1, informational==1) %>% 
  group_by(person_id) %>% 
  summarise(tot_inform_comp=sum(off_comp),tot_inform_view=sum(off_view), tot_inform_rec=sum(off_rec)) %>% 
  mutate(perc_inform_view=tot_inform_view/tot_inform_rec)

ggplot(inform_df, aes(perc_inform_view)) +
  geom_histogram()

inform_df %>% group_by(perc_inform_view) %>% count()

inform_df %>% count()


