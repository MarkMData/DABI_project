library(tidyverse)
library(magrittr)
library(GGally)

profile <- read.csv('profile.csv')
portolio <- read.csv('portfolio.csv')
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

# happy days
