library(tidyverse)
library(magrittr)



################################################################################
# loading data and preliminary wrangling
################################################################################

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

# Changing the NA values in reward and amount to zero
transcript$reward_rec[is.na(transcript$reward_rec)] <- 0
transcript$amount[is.na(transcript$amount)] <- 0

# Joining some of the columns from portfolio to transcript to help with matching 
# transactions and offers
transcript2 <- left_join(
  transcript, portfolio[,c('offer_id','duration', 'offer_num')], by = 'offer_id')

# joining with profile data
transcript2 <- left_join(transcript2, profile, by= 'person_id')

# dropping age = 118 observations
transcript2 <- transcript2 |> filter(age != 118)

# removing duplicate rows
transcript2 <- unique(transcript2)

################################################################################
# is there a difference in total spend per person between those who viewed the info
# offer and those that did not?
################################################################################

# Finding people who viewed info offers
info_people_df <- transcript2 |>
  filter((offer_num == 'offer3' | offer_num == 'offer8') & offer_viewed ==1) 
info_people <- unique(info_people_df$person_id)

# creating DF without people who viewed info offers
no_info_df <- transcript2 |>
  filter(!(person_id %in% info_people )) |>
  group_by(person_id) |>
  summarise(amount = sum(amount))
# creating DF with only people who viewed info offers
info_df <- transcript2 |>
  filter(person_id %in% info_people )|>
  group_by(person_id) |>
  summarise(amount = sum(amount))

# looking at means
summary(info_df$amount)
summary(no_info_df$amount) # those who viewed info spent slightly more on average

# Is there a sig difference between groups?
t.test(log(info_df$amount+1), log(no_info_df$amount+1))  # sig diff
wilcox.test(info_df$amount, no_info_df$amount) # sig diff
ks.test(info_df$amount, no_info_df$amount) # sig diff
# there is a small significant difference between groups

# distributions of log(amounts) for both groups
ggplot() +
  geom_density(data = no_info_df, aes(x = log(amount+1), color = "did not view info")) +
  geom_density(data = info_df, aes(x = log(amount+1), color = "viewed info")) +
  labs(x = "log amount", y = "Density") +
  scale_color_manual(values = c("blue", "red")) +  theme_minimal()

################################################################################
# Merging transactions and offers
################################################################################
# NOTE: this is does NOT include info offers

# Creating a DF with offers completed and transactions matched by time
offer_tran_df <- transcript2 |>
  filter(offer_completed == 1 | transaction == 1) |>
  group_by(person_id, time) |>
  summarise(
    offer_completed = sum(offer_completed),
    transaction = sum(transaction),
    reward_rec = sum(reward_rec),
    amount = sum(amount),
    offers = paste(offer_num[!is.na(offer_num)], collapse = ", ")
  )

# what percent of transactions are related to an offer
nrow(offer_tran_df[offer_tran_df$offer_completed>0,])/
  nrow(offer_tran_df[offer_tran_df$offer_completed<1,]) # 31%

# looking at average spend completing offer vs average spend out of offer

# Df of transactions outside of offers
trans_out_df <- offer_tran_df |>
  filter(offer_completed < 1) |>
  group_by(person_id) |>
  summarise(ave_amount_out = round(mean(amount), digits=2),
            tot_trans_out = round(sum(transaction)))
# Df of transactions completing an offer
trans_in_df <- offer_tran_df |>
  filter(offer_completed > 0) |>
  group_by(person_id) |>
  summarise(ave_amount_in = round(mean(amount), digits=2),
            tot_trans_in = round(sum(transaction)))

# Joining the two DFs
trans_in_out_df <- full_join(trans_in_df, trans_out_df, by= 'person_id')
trans_in_out_df[is.na(trans_in_out_df)] <- 0

# total average spend per transaction for all customers in and out of offer
trans_in_out_df |> select(ave_amount_in, ave_amount_out) |> colSums()

# is there a difference in average spend per person in and out of offers?
t.test(log(trans_in_out_df$ave_amount_in+1), log(trans_in_out_df$ave_amount_out+1))  # sig diff
wilcox.test(trans_in_out_df$ave_amount_in, trans_in_out_df$ave_amount_out) # sig diff
ks.test(trans_in_out_df$ave_amount_in, trans_in_out_df$ave_amount_out) # sig diff

#################################################################################
# Creating some summaries by offer
################################################################################

bogos <- portfolio[portfolio$bogo==1,]$offer_num
disc <- portfolio[portfolio$discount==1,]$offer_num
info <- portfolio[portfolio$informational==1,]$offer_num

mob <- portfolio[portfolio$mobile==1,]$offer_num
social <- portfolio[portfolio$social==1,]$offer_num
web <- portfolio[portfolio$web==1,]$offer_num

offer_summaries <- transcript2 |>
  mutate(
    bogo_rec = ifelse(
      offer_received == 1 & offer_num %in% bogos,1,0),
    disc_rec = ifelse(
      offer_received == 1 & offer_num %in% disc,1,0),
    info_rec = ifelse(
      offer_received == 1 & offer_num %in% info,1,0),
    bogo_view = ifelse(
      offer_viewed == 1 & offer_num %in% bogos,1,0),
    disc_view = ifelse(
      offer_viewed == 1 & offer_num %in% disc,1,0),
    info_view = ifelse(
      offer_viewed == 1 & offer_num %in% info,1,0),
    bogo_comp = ifelse(
      offer_completed == 1 & offer_num %in% bogos,1,0),
    disc_comp = ifelse(
      offer_completed == 1 & offer_num %in% disc,1,0),
    mob_rec = ifelse(
      offer_received == 1 & offer_num %in% mob,1,0),
    social_rec = ifelse(
      offer_received == 1 & offer_num %in% social,1,0),
    web_rec = ifelse(
      offer_received == 1 & offer_num %in% web,1,0),
    mob_view = ifelse(
      offer_viewed == 1 & offer_num %in% mob,1,0),
    social_view = ifelse(
      offer_viewed == 1 & offer_num %in% social,1,0),
    web_view = ifelse(
      offer_viewed == 1 & offer_num %in% web,1,0),
    mob_comp = ifelse(
      offer_completed == 1 & offer_num %in% mob,1,0),
    social_comp = ifelse(
      offer_completed == 1 & offer_num %in% social,1,0),
    web_comp = ifelse(
      offer_completed == 1 & offer_num %in% web,1,0)
  ) |>
  group_by(person_id) |>
  summarise(tot_reward_rec = sum(reward_rec),
            bogo_rec = sum(bogo_rec),
            disc_rec = sum(disc_rec),
            info_rec = sum(info_rec),
            bogo_view = sum(bogo_view),
            disc_view = sum(disc_view),
            info_view = sum(info_view),
            bogo_comp = sum(bogo_comp),
            disc_comp = sum(disc_comp),
            mob_rec = sum(mob_rec),
            social_rec = sum(social_rec),
            web_rec = sum(web_rec),
            mob_view = sum(mob_view),
            social_view = sum(social_view),
            web_view = sum(web_view),
            mob_comp = sum(mob_comp),
            social_comp = sum(social_comp),
            web_comp = sum(web_comp)
  )


# Reading in the old data wide file to get the overall totals
data_wide <- read.csv('data_wide2.1.csv')

# fixing ave_amount variable
data_wide$ave_amount <- round(data_wide$tot_amount/data_wide$tot_trans, digits = 2)

# Joing with new variables
data_wide <- full_join(data_wide, trans_in_out_df)
data_wide <- full_join(data_wide, offer_summaries)
data_wide[is.na(data_wide)] <- 0

# Checking duplicates
duplicates <- data_wide[duplicated(data_wide) |
                          duplicated(data_wide, fromLast = TRUE), ]
length(duplicates$person_id)

# Sanity checking new variables
sum(data_wide$tot_off_rec)-
  sum(data_wide$bogo_rec) -
  sum(data_wide$disc_rec) -
  sum(data_wide$info_rec) # ok

sum(data_wide$tot_trans) -
  sum(data_wide$tot_trans_in) -
  sum(data_wide$tot_trans_out) # ok


write.csv(data_wide, 'data_wide4.csv')
