library(tidyverse)
library(magrittr)
library(GGally)
library(stringr)
library(Hmisc)

profile <- read.csv('profile.csv')
portfolio <- read.csv('portfolio.csv')
transcript <- read.csv('transcript.csv')

######### Looking at transaction amounts ########
amounts <- transcript |>
  filter(transaction == 1) |>
  select(amount)
summary(amounts)
# transaction amounts are very skewed

# density plot of transaction amounts
ggplot(amounts)+
  geom_density(aes(x=amount))
ggplot(amounts)+
  geom_density(aes(x=log(amount)))


# Extracting the total, mean, max spend per customer
transactions <- transcript |>
  filter(transaction==1) |>
  group_by(person_id) |>
  summarise(total_spend = sum(amount),
            ave_spend = round(mean(amount),digits=2),
            max_spend = max(amount)) |>
  rename(id = person_id)


# adding the transaction data to the profile df
profile <- left_join(profile, transactions, by = 'id')

# Changing the na values in the transaction summaries to zeros
profile$total_spend[is.na(profile$total_spend)] <- 0
profile$ave_spend[is.na(profile$ave_spend)] <- 0
profile$max_spend[is.na(profile$max_spend)] <- 0

# dropping the X column
profile$X <- NULL

# Checking for any duplicate values of customer id
profile[duplicated(profile$id), ]

# dropping observations with age = 118
profile <- profile[profile$age!=118,]

# finding incomplete cases
profile[!complete.cases(profile),]

# looking at number of customers who did not spend any money
length(profile$total_spend[profile$total_spend==0])
# looking at number of customers who spent less than £1
length(profile$total_spend[profile$total_spend<1])

# creating tenure variable
profile$tenure <- as.integer(
  difftime(max(profile$membership_start),
           profile$membership_start, unit = 'days')+1
)

summary(profile)

######### Visualizations ##########
# gender bar chart
ggplot(profile,aes(gender))+
  geom_bar(aes(fill = gender))+
  theme_classic()

# frequency polygons by gender
# age
ggplot(profile)+
  geom_freqpoly(aes(age, color = gender),linewidth = 1)+
  theme_classic()

# income
ggplot(profile)+
  geom_freqpoly(aes(income, color = gender),linewidth = 1)+
  theme_classic()

# tenure
ggplot(profile)+
  geom_freqpoly(aes(tenure, color = gender),linewidth = 1)+
  theme_classic()

# log(Total spend)
ggplot(profile)+
  geom_freqpoly(aes(log(total_spend), color = gender),linewidth = 1)+
  theme_classic()

# log(Max spend)
ggplot(profile)+
  geom_freqpoly(aes(log(max_spend), color = gender),linewidth = 1)+
  theme_classic()

# log(Average spend)
ggplot(profile)+
  geom_freqpoly(aes(log(ave_spend), color = gender),linewidth = 1)+
  theme_classic()


# pairs plot
ggpairs(profile, columns = c('age', 'income', 'tenure', 'total_spend',
                             'ave_spend', 'max_spend') ,
        aes(color = gender, alpha = 0.5))+
  theme_classic()

###### TRANSCRIPT ANALYSIS ######

# summary of transcript
summary(transcript)
str(transcript)

# create average amount per individual
average_transaction<-transcript %>% 
      select(person_id,amount) %>% 
      group_by(person_id) %>% 
      summarise(mean_transaction=mean(amount, na.rm = TRUE)) %>% 
      arrange(desc(mean_transaction))

# plot hist average amount per individual
ggplot(data=average_transaction, aes(mean_transaction))+
      geom_histogram(bins=100)

ggplot(data=average_transaction, aes(mean_transaction))+
      geom_histogram(bins=100)+
      xlim(c(0,50))


# create vector total amount per person
total_transaction<-transcript %>% 
      select(person_id,amount) %>% 
      group_by(person_id) %>% 
      summarise(total_transactions=sum(amount, na.rm = TRUE))

# plot total amount per individual
ggplot(data=total_transaction, aes(total_transactions))+
      geom_histogram(bins=100)+
      xlim(c(0,500))

# plot total amount per individual on log scale
ggplot(data=total_transaction, aes(log(total_transactions)))+
      geom_histogram(bins=100)


# plot number of each time frame appears in transcript
ggplot(data=transcript, aes(time))+
      geom_histogram()

# create vector number of transactions per individual (people with zero transactions dropped)
number_transaction<-transcript %>% 
      select(person_id,amount) %>% 
      drop_na() %>% 
      group_by(person_id) %>% 
      count()

# number of offers received per person
offers_received <- transcript %>% 
      select(person_id, offer_received) %>% 
      filter(offer_received==1) %>% 
      group_by(person_id) %>% 
      count()
offers_received

# merge no. of transactions with offers received
data<-merge(number_transaction,offers_received, by="person_id")
# boxplot of transactions and  offers received
ggplot(data=data, aes(y=n.x))+
      geom_boxplot(aes(fill=factor(n.y)))+
      xlab("offers received")+
      ylab("no. of transactions")

# dataframe of offer viewed and offer completed
offer_count<-transcript %>% 
      filter(offer_id != "") %>% 
      select(offer_id, offer_received, offer_viewed, offer_completed) %>% 
      group_by(offer_id) %>% 
      summarise(or_count=sum(offer_received),ov_count=sum(offer_viewed),oc_count=sum(offer_completed)) %>% 
      mutate(ov_perc=ov_count/or_count, oc_perc=oc_count/or_count)
offer_count



# # percentage offer viewed and offer completed
ggplot(data=offer_count, aes(ov_perc, oc_perc))+
      geom_point()+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))

# merge offer count with portfolio
offer_count<-merge(offer_count, portfolio, by.x="offer_id", by.y="id")
offer_count

# offers viewed and completed comparison bogo
ggplot(data=offer_count, aes(ov_perc, oc_perc, col=factor(bogo)))+
      geom_point(size=3)+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))

# offers viewed and completed comparison discount
ggplot(data=offer_count, aes(ov_perc, oc_perc, col=factor(discount)))+
      geom_point(size=3)+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))

# offers viewed and completed comparison information
ggplot(data=offer_count, aes(ov_perc, oc_perc, col=factor(informational)))+
      geom_point(size=3)+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))

# offers viewed and completed comparison difficulty
ggplot(data=offer_count, aes(ov_perc, oc_perc, col=factor(difficulty)))+
      geom_point(size=3)+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))

# offers viewed and completed comparison reward
ggplot(data=offer_count, aes(ov_perc, oc_perc, col=factor(reward)))+
      geom_point(size=3)+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))

# offers viewed and completed comparison reward
ggplot(data=offer_count, aes(ov_perc, oc_perc, col=factor(difficulty-reward)))+
      geom_point(size=3)+
      geom_abline(slope=1,intercept=0)+
      xlim(c(0,1))+
      ylim(c(0,1))


#######  RFM  #########

# merge transcript and profile with inner join to remove people aged 118
transcript_profile<-merge(transcript, profile, by.x="person_id", by.y="id")

dim(transcript)
dim(transcript_profile)


colnames(transcript_profile)

# check for duplicates
duplicates <- transcript_profile[duplicated(transcript_profile) |
                                duplicated(transcript_profile, fromLast = TRUE), ]

duplicates
# total monetary value column for every person
monetary_value <- transcript_profile %>% 
      group_by(person_id) %>% 
      slice(1) %>% 
      select(total_spend)
dim(profile)
dim(monetary_value)
# total frequency column for every person
number_transaction<-transcript_profile %>% 
      select(person_id,transaction) %>% 
      group_by(person_id) %>% 
      summarise(sum(transaction))



# recency column
recency <- transcript_profile %>% 
      select(person_id,time, transaction) %>%
      filter(transaction==1) %>% 
      group_by(person_id) %>% 
      summarise(recency=max(time))

# check if customers with no transactions removed
dim(profile)
dim(monetary_value)
dim(number_transaction)
dim(recency)   

# merge rfm values and rename columns
rfm_table<-right_join(recency, number_transaction, by="person_id", )
rfm_table<-inner_join(rfm_table, monetary_value, by="person_id")

head(rfm_table)

# rename columns
colnames(rfm_table)<-c( "person_id","recency","frequency","monetary_value")


# check minimum in recency
rfm_table %>% 
      drop_na(recency) %>% 
      summarise(min(recency))

# change na values to 0
rfm_table$recency[is.na(rfm_table$recency)]<-0

profile

# function to create rfm table
make_rfm<-function(factors){
      # create vector totalmoney spent
      monetary_value <- transcript_profile %>% 
            group_by(person_id) %>% 
            slice(1) %>% 
            dplyr::select(total_spend)
      # create vector frequecy of transactions
      frequency<-transcript_profile %>% 
            dplyr::select(person_id,transaction) %>% 
            group_by(person_id) %>% 
            summarise(sum(transaction))
      # create vector recency of transactions
      recency <- transcript_profile %>% 
            dplyr::select(person_id,time, transaction) %>%
            filter(transaction==1) %>% 
            group_by(person_id) %>% 
            summarise(recency=max(time))
      # join 3 columns (right join with recency as it loses people who don't have a transaction)
      rfm_table<-right_join(recency, frequency, by="person_id")
      rfm_table<-inner_join(rfm_table, monetary_value, by="person_id")
      # change colnames
      colnames(rfm_table)<-c( "person_id","recency","frequency","monetary_value")
      # fill recency with 0 for people who have not had a transaction
      rfm_table$recency[is.na(rfm_table$recency)]<-0
      # create rfm values by creating factors based on number of breaks chosen in function
      rfm_table["r_score"]<-factor(Hmisc::cut2(rfm_table$recency, cuts=c(quantile(rfm_table$recency, probs = seq(0, 1, by = 1/factors))), g = factors), labels = c(1:factors))
      rfm_table["f_score"]<- factor(Hmisc::cut2(rfm_table$frequency, cuts=c(quantile(rfm_table$frequency, probs = seq(0, 1, by = 1/factors))), g = factors), labels = c(1:factors))
      rfm_table["m_score"]<- factor(Hmisc::cut2(rfm_table$monetary_value, cuts=c(quantile(rfm_table$monetary_value, probs = seq(0, 1, by = 1/factors))), g = factors), labels = c(1:factors))
      # create fm score and string adding f_score with m_score
      rfm_table["fm_string"]<-str_c(rfm_table$f_score,rfm_table$m_score)
      rfm_table["fm_score"]<-as.numeric(rfm_table$f_score)+as.numeric(rfm_table$m_score)
      # create rfm score and string adding r_score with f_score and m_score
      rfm_table["rfm_string"]<-str_c(rfm_table$r_score,rfm_table$f_score,rfm_table$m_score)
      rfm_table["rfm_score"]<-as.numeric(rfm_table$r_score)+as.numeric(rfm_table$f_score)+as.numeric(rfm_table$m_score)
      # return merge with profile data frame removing total spend asduplicated with monetary value
      return(merge(profile,rfm_table, by.x="id", by.y="person_id") %>% dplyr::select(-total_spend))
}

# create rfm table splitting into 
rfm_table<-make_rfm(10)
rfm_table

# cut offers into quantile ranges of 5

# plot factor split
ggplot(data=rfm_table, aes(recency))+
      geom_histogram(aes(fill=r_score))

ggplot(data=rfm_table, aes(frequency))+
      geom_histogram(aes(fill=f_score))

ggplot(data=rfm_table, aes(monetary_value))+
      geom_histogram(aes(fill=m_score))

# check groups equally split
rfm_table %>% group_by(r_score) %>% 
      count()

rfm_table %>% group_by(f_score) %>% 
      count()

rfm_table %>% group_by(m_score) %>% 
      count()

# check group split
rfm_table %>% group_by(rfm_string) %>% 
      count() %>% 
      arrange(desc(n))

rfm_table %>% group_by(fm_string) %>% 
      count() %>% 
      arrange(desc(n))

# check rfm, fm and r, f and m change with gender
ggplot(rfm_table,aes(y=rfm_score))+
      geom_boxplot(aes(fill=gender))
ggplot(rfm_table,aes(y=fm_score))+
      geom_boxplot(aes(fill=gender))
ggplot(rfm_table,aes(y=as.numeric(r_score)))+
      geom_boxplot(aes(fill=gender))+
      ylab("r_score")+
      xlab("")+
      theme_classic() +
      theme_update(axis.ticks.x = element_blank(),
                   axis.text.x = element_blank())

ggplot(rfm_table,aes(y=as.numeric(f_score)))+
      geom_boxplot(aes(fill=gender))+
      ylab("f_score")+
      xlab("")+
      theme_classic() +
      theme_update(axis.ticks.x = element_blank(),
                   axis.text.x = element_blank())

# largest difference where malesaveragely spend less
ggplot(rfm_table,aes(y=as.numeric(m_score)))+
      geom_boxplot(aes(fill=gender))+
      ylab("m_score")+
      xlab("")+
      theme_classic() +
      theme_update(axis.ticks.x = element_blank(),
                   axis.text.x = element_blank())


# create age factor
summary(rfm_table$age)
age_factor<-cut(rfm_table$age, breaks=c(17, 29, 39, 49, 59, 69, 101))
age_label<-c('18-29', '30-39', '40-49', '50-59','60-69',"70+")
#plot changes in r, m, f with age
ggplot(rfm_table,aes(y=rfm_score))+
      geom_boxplot(aes(fill=age_factor))+
      scale_fill_discrete(labels=age_label)

ggplot(rfm_table,aes(y=fm_score))+
      geom_boxplot(aes(fill=age_factor))+
      scale_fill_discrete(labels=age_label)

ggplot(rfm_table,aes(y=as.numeric(r_score)))+
      geom_boxplot(aes(fill=age_factor))+
      scale_fill_discrete(labels=age_label)
# young people appear to be more frequent
ggplot(rfm_table,aes(y=as.numeric(f_score)))+
      geom_boxplot(aes(fill=age_factor))+
      scale_fill_discrete(labels=age_label)

#older people total spend is more
ggplot(rfm_table,aes(y=as.numeric(m_score)))+
      geom_boxplot(aes(fill=age_factor))+
      scale_fill_discrete(labels=age_label)


# check income range
quantile(rfm_table$income, by=c(0,0.25,.5,0.75,1))
# make income factor
income_factor<-cut(rfm_table$income,breaks=c(29999,39999,49999,59999,69999, 79999,89999,120000))
income_label<-c('$30000-$39999', '$40000-$49999', '$50000-$59999', '$60000-$69999',
                '$70000-$79999','$80000-$89999','$90000-$120000')

# plot rfm_score compared to income
ggplot(rfm_table,aes(y=rfm_score))+
      geom_boxplot(aes(fill=income_factor))+
      scale_fill_discrete(labels=income_label)

ggplot(rfm_table,aes(y=fm_score))+
      geom_boxplot(aes(fill=income_factor))+
      scale_fill_discrete(labels=income_label)


ggplot(rfm_table,aes(y=as.numeric(r_score)))+
      geom_boxplot(aes(fill=income_factor))+
      scale_fill_discrete(labels=income_label)
# lower earners go more frequently
ggplot(rfm_table,aes(y=as.numeric(f_score)))+
      geom_boxplot(aes(fill=income_factor))+
      scale_fill_discrete(labels=income_label)
# higher earners spend larger amounts
ggplot(rfm_table,aes(y=as.numeric(m_score)))+
      geom_boxplot(aes(fill=income_factor))+
      scale_fill_discrete(labels=income_label)

# check if high spends are outliers
sort(rfm_table$monetary_value, decreasing=TRUE)[1:20]

# check if max spent are outliers
sort(rfm_table$max_spend, decreasing = TRUE)[1:20]


ggplot(data=rfm_table, aes(age, income))+
      geom_point()
cor(rfm_table$age,rfm_table$income)
working_age_rfm<-rfm_table %>% filter(age<65)
cor(working_age_rfm$age, working_age_rfm$income)
working_age_rfm<-rfm_table %>% filter(age<60)
cor(working_age_rfm$age, working_age_rfm$income)


summary(income_factor)
summary(age_factor)

colnames(transcript)
transactions<-transcript %>% filter(transaction==1)
colnames(transactions)
ggplot(data=transactions, aes(time)) +
  geom_histogram()


colnames(rfm_table)
rfm_table["person_id"]<-rfm_table$id
transactions_rfm<-merge(transactions, rfm_table, by.x="person_id", by.y="id")
colnames(transactions_rfm)

ggplot(data=transactions_rfm, aes(time)) +
  geom_histogram()+
  facet_wrap(vars(f_score))


ggplot(data=transactions_rfm, aes(time)) +
  geom_histogram()+
  facet_wrap(vars(m_score))

