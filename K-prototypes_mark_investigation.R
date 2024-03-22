library(tidyverse)
library(gridExtra)

train_df<-read.csv("kproto_train_data.csv")
test_df<-read.csv("kproto_test_data.csv")
complete_df<-read.csv("kproto_complete_data.csv")

complete_df$Cluster4<- as.factor(complete_df$Cluster4)

clust_colmap4 = c("#f7286d","#1faae0","#ffbf1f","#98FB98")
################################################################################
# Looking at 4 clusters training set
################################################################################
complete_df %>% group_by(Cluster4) %>% count()

# gender bar chart
ggplot(complete_df,aes(Cluster4))+
  geom_bar(aes(Cluster4, fill=gender), alpha = 0.5)+
  geom_text(stat = 'count',
            aes(label=percent(after_stat(count)/nrow(complete_df))),
            size = 4, 
            fontface = "bold")+
  labs(title = "Training set", x = "", y = "") 

complete_df %>% group_by(Cluster4,gender) %>% count() %>% ungroup(gender) %>% mutate(percentage=n/sum(n))

####  RMF ###################################### 
complete_df %>%gather(c(
  r_score,f_score,m_score
),  
key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_boxplot() +
  facet_wrap(~Para,ncol=3,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")



# Frequency vs. Monetary Score Scatter Plot
ggplot(complete_df, aes(x = f_score, y = m_score, color = Cluster4)) +
  geom_point(alpha = 0.6,position = "jitter") +
  theme_minimal() +
  labs(title = "Training set", x = "Frequency Score", y = "Monetary Score") +
  scale_fill_manual(values=clust_colmap4)

complete_df %>% group_by(Cluster4) %>% summarise(mean(f_score),median(f_score),mean(m_score),median(m_score))
####### demographics, overall summaries ###########
# violins
complete_df %>%gather(c(
  age,income, tenure, tot_off_rec, tot_off_comp,
  log_tot_amount, log_ave_amount, log_max_amount, log_ave_amount_in, log_ave_amount_out,
  tot_trans, tot_trans_in, tot_trans_out, reward_off, tot_reward_rec
),  
key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=5,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")


##### bogo offers #########
bogo <- complete_df %>% filter(bogo_rec!=0)

# violins
bogo |>
  gather(c(
    bogo_rec, bogo_view, bogo_comp,
    bogo_view_rate, bogo_response_rate,
  ), 
  key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=5,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")

##### disc offers #########

disc <- complete_df %>% filter(disc_rec!=0) 
# violins
disc |>
  gather(c(
    disc_rec, disc_view, disc_comp,
    disc_view_rate, disc_response_rate,
  ),  
  key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=5,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")


##### info offers #########

info <- complete_df %>% filter(info_rec!=0)

# boxplots
info |>
  gather(c(
    info_rec, info_view,info_view_rate
  ),
  key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=3,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")


##### web offers #########
web <- complete_df %>% filter(web_rec!=0)
# violins
web |>
  gather(c(
    web_rec, web_view, web_comp, web_view_rate, web_comp_rate
  ), 
  key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=5,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")


##### mob offers #########
mob <- complete_df %>% filter(mob_rec!=0)
# violins
mob |>
  gather(c(
    mob_rec, mob_view, mob_comp, mob_view_rate, mob_comp_rate
  ), 
  key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=5,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")

##### social offers #########
soc <- complete_df %>% filter(social_rec!=0)
# violins
soc |>
  gather(c(
    social_rec, social_view, social_comp, social_view_rate, social_comp_rate
  ), 
  key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=5,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap4)+
  theme(legend.position = "none")


# Summary table ###############################################################

four_clust_summary <- complete_df %>%
  group_by(Cluster4) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    income = mean(income, na.rm = TRUE),
    tenure = mean(tenure, na.rm = TRUE),
    ave_amount = mean(ave_amount, na.rm = TRUE),
    ave_amount_in = mean(ave_amount_in, na.rm = TRUE),
    ave_amount_out = mean(ave_amount_out, na.rm = TRUE),
    tot_amount = mean(tot_amount, na.rm = TRUE),
    tot_off_rec = mean(tot_off_rec, na.rm = TRUE),
    offer_completion_rate=mean(offer_completion_rate, na.rm=TRUE),
    offer_view_rate= mean(offer_view_rate, na.rm = TRUE),
    tot_off_comp = mean(tot_off_comp, na.rm = TRUE),
    tot_trans = mean(tot_trans, na.rm = TRUE),
    tot_trans_in = mean(tot_trans_in, na.rm = TRUE),
    bogo_response_rate = mean(bogo_response_rate, na.rm = TRUE),
    disc_response_rate = mean(disc_response_rate, na.rm = TRUE),
    info_view_rate = mean(info_view_rate, na.rm = TRUE),
    web_comp_rate = mean(web_comp_rate, na.rm = TRUE),
    mob_comp_rate = mean(mob_comp_rate, na.rm = TRUE),
    social_comp_rate = mean(social_comp_rate, na.rm = TRUE),
    f_score = mean(f_score, na.rm = TRUE),
    m_score = mean(m_score, na.rm = TRUE)
  )

# Pivot the dataframe longer
longer_df <- four_clust_summary %>%
  pivot_longer(cols = -Cluster4,
               names_to = "Variable",
               values_to = "Value")

wider_df <- longer_df |>
  pivot_wider(names_from = Cluster4, values_from = Value)
wider_df %>% print(n=Inf)


#cluster size n
complete_df %>% group_by(Cluster4) %>% count()

# Plot age
ggplot(complete_df, aes(age_group))+
  geom_bar(aes(fill=Cluster4))
ggplot(complete_df, aes(age))+
  geom_boxplot(aes(fill=Cluster4))
complete_df %>% filter(Cluster4==3) %>% reframe(quantile(age, probs=c(0.05,0.25,0.50,0.75, 0.95))) 
complete_df$age_group<-factor(complete_df$age_group,levels= c("<=30","31-40","41-50","51-60","61-70","71+"))
complete_df %>% group_by(Cluster4, age_group) %>% count() %>% 
  ungroup(age_group) %>% mutate(n/sum(n), cumsum(n)/sum(n))%>%
  print(n=Inf)
# plot income

ggplot(complete_df, aes(income_bracket))+
  geom_bar(aes(fill=Cluster4))
ggplot(complete_df, aes(income))+
  geom_boxplot(aes(fill=Cluster4))
complete_df %>% filter(Cluster4==4) %>% reframe(quantile(income, probs=c(0.05,0.25,0.50,0.75, 0.95))) 
complete_df %>% group_by(Cluster4, income_bracket) %>% count() %>% 
  ungroup(income_bracket) %>% mutate(n/sum(n), cumsum(n)/sum(n))%>%
  print(n=Inf)
# plot tenure
ggplot(complete_df, aes(tenure))+
  geom_boxplot(aes(fill=Cluster4))

# plot average amount
ggplot(complete_df, aes(ave_amount))+
  geom_boxplot(aes(fill=Cluster4))

ggplot(complete_df, aes(ave_amount))+
  geom_boxplot(aes(fill=Cluster4))+
  xlim(c(0,200))

ggplot(complete_df, aes(log_ave_amount))+
  geom_boxplot(aes(fill=Cluster4))
max(complete_df$ave_amount)
complete_df %>% filter(Cluster4==4) %>% reframe(quantile(ave_amount, probs=c(0.05,0.25,0.50,0.75, 0.95))) 

# transactions
ggplot(complete_df, aes(tot_trans))+
  geom_boxplot(aes(fill=Cluster4))
complete_df %>% filter(Cluster4==4) %>% reframe(quantile(tot_trans, probs=c(0.05,0.25,0.50,0.75, 0.95))) 


######################################
# load transcript and portfolio data
transcript2<-read.csv("transcript2.csv")
portfolio<-read.csv("portfolio2.csv")
clust_comp<-complete_df %>% select(person_id, Cluster4)


# merge train and test data
comp_transcript<- merge(transcript2, clust_comp, by="person_id")


#########################################################################
# percentage offer completed on complete data with difficulty and reward data
offer_diff<-comp_transcript %>% 
  group_by(Cluster4, offer_num) %>% 
  summarise(off_comp= sum(offer_completed), off_rec=sum(offer_received)) %>% 
  ungroup() %>% 
  mutate(perc_off_comp=off_comp/off_rec)

# merge with portfolio and remove informational offers
offer_diff_comp<-merge(offer_diff, portfolio %>% select(difficulty, reward_off, duration, offer_num),by="offer_num")
offer_diff_comp<- offer_diff_comp %>% filter(reward_off!=0)

# change offer_number to bogo or discount
for(i in 1:dim(offer_diff_comp)[1]){
  if(offer_diff_comp$offer_num[i]=="offer1"){
    offer_diff_comp$offer_num[i]<-"bogo1"
  }
  else if(offer_diff_comp$offer_num[i]=="offer2"){
    offer_diff_comp$offer_num[i]<-"bogo2"
  }
  else if(offer_diff_comp$offer_num[i]=="offer4"){
    offer_diff_comp$offer_num[i]<-"bogo3"
  }
  else if(offer_diff_comp$offer_num[i]=="offer5"){
    offer_diff_comp$offer_num[i]<-"discount1"
  }
  else if(offer_diff_comp$offer_num[i]=="offer6"){
    offer_diff_comp$offer_num[i]<-"discount2"
  }
  else if(offer_diff_comp$offer_num[i]=="offer7"){
    offer_diff_comp$offer_num[i]<-"discount3"
  }
  else if(offer_diff_comp$offer_num[i]=="offer9"){
    offer_diff_comp$offer_num[i]<-"bogo4"
  }
  else if(offer_diff_comp$offer_num[i]=="offer10"){
    offer_diff_comp$offer_num[i]<-"discount4"
  }
}



# plot interactions with offers and cluster and reward
p1<-ggplot(offer_diff_comp %>% filter(Cluster4==1), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(reward_off)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 1 interaction with offers")+
  ylab("Percentage of Offers Completed")+
  theme(legend.position="none",axis.text.x = element_blank())

p2<-ggplot(offer_diff_comp %>% filter(Cluster4==2), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(reward_off)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 2 interaction with offers")+
  ylab("")+
  theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank())+
  guides(fill=guide_legend(title="Difficulty"))

p3<-ggplot(offer_diff_comp %>% filter(Cluster4==3), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(reward_off)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 3 interaction with offers")+
  theme(legend.position = c(0.9, 0.8))+
  ylab("Percentage of Offers Completed")+
  guides(fill=guide_legend(title="Reward"))

p4<-ggplot(offer_diff_comp %>% filter(Cluster4==4), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(reward_off)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.text.y = element_blank() )+
  xlab("")+
  ggtitle("Cluster 4 interaction with offers")+
  ylab("")+
  theme(legend.position="none")

grid.arrange(p1,p2,p3,p4)

# plot interactions with offers and cluster and difficulty
p1<-ggplot(offer_diff_comp %>% filter(Cluster4==1), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(difficulty)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 1 interaction with offers")+
  ylab("Percentage of Offers Completed")+
  theme(legend.position="none",axis.text.x = element_blank())

p2<-ggplot(offer_diff_comp %>% filter(Cluster4==2), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(difficulty)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 2 interaction with offers")+
  ylab("")+
  theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank())+
  guides(fill=guide_legend(title="Difficulty"))

p3<-ggplot(offer_diff_comp %>% filter(Cluster4==3), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(difficulty)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 3 interaction with offers")+
  ylab("Percentage of Offers Completed")+
  theme(legend.position = c(0.9, 0.8))+
  guides(fill=guide_legend(title="Difficulty"))

p4<-ggplot(offer_diff_comp %>% filter(Cluster4==4), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(difficulty)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.text.y = element_blank() )+
  xlab("")+
  ggtitle("Cluster 4 interaction with offers")+
  ylab("")+
  theme(legend.position="none")

grid.arrange(p1,p2,p3,p4)


# plot interactions with offers and cluster and difficulty
p1<-ggplot(offer_diff_comp %>% filter(Cluster4==1), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(duration)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 1 interaction with offers")+
  ylab("Percentage of Offers Completed")+
  theme(legend.position="none",axis.text.x = element_blank())

p2<-ggplot(offer_diff_comp %>% filter(Cluster4==2), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(duration)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 2 interaction with offers")+
  ylab("")+
  theme(legend.position="none",axis.text.x = element_blank(),axis.text.y = element_blank())+
  guides(fill=guide_legend(title="Duration in Hours"))

p3<-ggplot(offer_diff_comp %>% filter(Cluster4==3), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(duration)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ggtitle("Cluster 3 interaction with offers")+
  ylab("Percentage of Offers Completed")+
  theme(legend.position = c(0.9, 0.8))+
  guides(fill=guide_legend(title="Duration in Hours"))

p4<-ggplot(offer_diff_comp %>% filter(Cluster4==4), aes(offer_num,perc_off_comp))+
  geom_bar(stat = "identity",aes(fill=factor(duration)))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.text.y = element_blank() )+
  xlab("")+
  ggtitle("Cluster 4 interaction with offers")+
  ylab("")+
  theme(legend.position="none")

grid.arrange(p1,p2,p3,p4)

#######################################################
# how each cluster reacts with viewing and completing bogo and discount offers
complete_df %>% group_by(Cluster4) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))

# group by cluster and offer type to see how transactions and amount vary with offers
period1<-complete_df %>% group_by(Cluster4, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1),n(), sqrt(var(tot_amount1)),sqrt(var(ave_amount1)))
period2<-complete_df %>% group_by(Cluster4, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2),n(),sqrt(var(tot_amount2)), sqrt(var(ave_amount2))) 
period3<-complete_df %>% group_by(Cluster4, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3),n(),sqrt(var(tot_amount3)), sqrt(var(ave_amount3)))
period4<-complete_df %>% group_by(Cluster4, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4),n(),sqrt(var(tot_amount4)), sqrt(var(ave_amount4))) 
period5<-complete_df %>% group_by(Cluster4, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5),n(),sqrt(var(tot_amount5)), sqrt(var(ave_amount5)))
period6<-complete_df %>% group_by(Cluster4, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6),n(),sqrt(var(tot_amount6)), sqrt(var(ave_amount6)))


# group to see how they vary throughout all transactions
total_offer_behaviour<- data.frame(period1$Cluster4, period1$offer_type1, period1[,3:6]+period2[,3:6]+period3[,3:6]+period4[,3:6]+period5[,3:6]+period6[,3:6])

colnames(total_offer_behaviour) <-c("cluster","offer_type","transactions","tot_amount", "ave_amount","n")
total_offer_behaviour %>% group_by(cluster) %>% mutate(perc_trans=transactions/sum(transactions),perc_tot_amount=tot_amount/sum(tot_amount)) %>% print(n=Inf)




##### check how cluster 1 spends

transcript2
