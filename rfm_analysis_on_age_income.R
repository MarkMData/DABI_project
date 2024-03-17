library(tidyverse)

data_wide5 <- read_csv("data_wide5.1.csv")

# check rfm, fm and r, f and m change with gender
ggplot(data_wide5,aes(y=rfm_score))+
  geom_boxplot(aes(fill=gender))

ggplot(data_wide5,aes(y=as.numeric(r_score)))+
  geom_boxplot(aes(fill=gender))+
  ylab("r_score")+
  xlab("")+
  theme_classic() +
  theme_update(axis.ticks.x = element_blank(),
               axis.text.x = element_blank())

ggplot(data_wide5,aes(y=as.numeric(f_score)))+
  geom_boxplot(aes(fill=gender))+
  ylab("f_score")+
  xlab("")+
  theme_classic() +
  theme_update(axis.ticks.x = element_blank(),
               axis.text.x = element_blank())

# largest difference where malesaveragely spend less
ggplot(data_wide5,aes(y=as.numeric(m_score)))+
  geom_boxplot(aes(fill=gender))+
  ylab("m_score")+
  xlab("")+
  theme_classic() +
  theme_update(axis.ticks.x = element_blank(),
               axis.text.x = element_blank())

colnames(data_wide5)

#plot changes in r, m, f with age
ggplot(data_wide5,aes(y=rfm_score))+
  geom_boxplot(aes(fill=age_group))



ggplot(data_wide5,aes(y=as.numeric(r_score)))+
  geom_boxplot(aes(fill=age_group))
# young people appear to be more frequent
ggplot(data_wide5,aes(y=as.numeric(f_score)))+
  geom_boxplot(aes(fill=age_group))

#older people total spend is more
ggplot(data_wide5,aes(y=as.numeric(m_score)))+
  geom_boxplot(aes(fill=age_group))



# plot rfm_score compared to income
ggplot(data_wide5,aes(y=rfm_score))+
  geom_boxplot(aes(fill=income_bracket))


ggplot(data_wide5,aes(y=as.numeric(r_score)))+
  geom_boxplot(aes(fill=income_bracket))
# lower earners go more frequently

ggplot(data_wide5,aes(y=as.numeric(f_score)))+
  geom_boxplot(aes(fill=income_bracket))
# higher earners spend larger amounts
ggplot(data_wide5,aes(y=as.numeric(m_score)))+
  geom_boxplot(aes(fill=income_bracket))



