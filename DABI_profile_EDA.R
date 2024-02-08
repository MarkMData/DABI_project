library(tidyverse)
library(magrittr)
library(GGally)

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

# total monetary value column for every person
colnames(transcript_profile)

monetary_value <- transcript_profile %>% 
      group_by(person_id) %>% 
      slice(1) %>% 
      select(total_spend)

# total frequency column for every person
number_transaction<-transcript_profile %>% 
      select(person_id,transaction) %>% 
      group_by(person_id) %>% 
      summarise(sum(transaction))

number_transaction

# recency column
recency <- transcript_profile %>% 
      select(person_id,time, transaction) %>%
      filter(transaction==1) %>% 
      group_by(person_id) %>% 
      summarise(recency=max(time))
recency      
