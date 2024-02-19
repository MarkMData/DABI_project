library(tidyverse)
library(magrittr)
library(GGally)


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

################################################################################
# looking at the separate offer periods
################################################################################

# Creating plot of daily transaction amount and counts with offers
offers_amount <- transcript2 |>
  filter(transaction ==1) |>
  group_by(time) |>
  summarise(amount = sum(amount),
            count = sum(transaction))

# getting times offers sent
offer_rec <- transcript2 |> filter(offer_received==1) 
unique(offer_rec$time)

ggplot(offers_amount, aes(x=time)) +
  geom_line(aes(y=amount), color= 'darkgreen')+
  geom_line(aes(y=count*10), color = 'blue')+
  scale_y_continuous(
    name = "Amount",
    sec.axis = sec_axis(~./10, name = "Count")
  ) +
  geom_vline(xintercept = unique(offer_rec$time))+
  theme_classic() 

### There is a difference between transactions and amount,
# probably worth considering both at both


# checking that customers only receive one offer at a time    
offers_cust <- transcript2 |>
  filter(offer_received == 1) |>
  group_by(person_id, time) |>
  summarise(count = sum(offer_received))
max(offers_cust$count) ### people only get 1 offer at a time ###

# Checking for duplicate values in transcript
duplicates <- transcript2[duplicated(transcript2) |
                            duplicated(transcript2, fromLast = TRUE), ]

length(duplicates$person_id) # 793 duplicate rows

# removing duplicate rows
transcript2 <- unique(transcript2)

# Splitting DF into offer periods
period1 <- transcript2 |> filter(time < 168)
period2 <- transcript2 |> filter(time >= 168 & time < 336)
period3 <- transcript2 |> filter(time >= 336 & time < 408)
period4 <- transcript2 |> filter(time >= 408 & time < 504)
period5 <- transcript2 |> filter(time >= 504 & time < 576)
period6 <- transcript2 |> filter(time >= 576)

#looking at number of transactions per customer for each period
trans_per_cust <- function(X){
  num_trans <- X |>
    group_by(person_id) |>
    summarise(num_trans = sum(transaction))
  print(summary(num_trans$num_trans))
  print(length(unique(X$person_id)))
  hist(num_trans$num_trans, xlab = 'Number of transactions',
       main = '')
}

trans_per_cust(period1)
trans_per_cust(period2)
trans_per_cust(period3)
trans_per_cust(period4)
trans_per_cust(period5)
trans_per_cust(period6)


################################################################################
# Creating summaries by person for each offer period
################################################################################

# function to link offers and transactions
offer_trans <- function(X){
  x1 <- X |>         # linking offer numbers to transactions
    filter(offer_received == 1) |>
    mutate(Offer_num = offer_num,
           Duration = duration) |>
    select(person_id, Offer_num, Duration)|>
    distinct()
  x2 <- left_join(X,x1, by = 'person_id')
  x3 <- x2 |>        # linking completed offers to transactions
    filter(offer_completed == 1) |> 
    mutate(Offer_completed = offer_completed) |>
    select(person_id, Offer_completed)|>
    distinct()
  x4 <- left_join(x2, x3, by = 'person_id')
  x5 <- x4 |>        # linking viewed offers to transactions
    filter(offer_viewed == 1) |>
    mutate(Offer_viewed = offer_viewed) |>
    select(person_id, Offer_viewed)|>
    distinct()
  x6 <- left_join(x4, x5, by = 'person_id')
  x6$Offer_completed <- x6$Offer_completed |> replace_na(0)
  x6$Offer_viewed <- x6$Offer_viewed |> replace_na(0)
  x6$Duration <- x6$Duration |> replace_na(0)
  x6$Offer_num <- x6$Offer_num |> replace_na('no_offer')
  
  x7 <- x6 |>
    mutate(
      # creating cols for transactions inside offers
      off_trans =
        ifelse(
          transaction == 1 & 
            time <= (Duration + min(time)) &
            Offer_completed == 1 & 
            Offer_num %in% c('offer1', 'offer2', 'offer4', 'offer5', 'offer6',
                             'offer7', 'offer9', 'offer10'), 1,
          ifelse(
            transaction == 1 &
              time <= (Duration + min(time)) &
              Offer_viewed == 1 &
              Offer_num %in% c('offer3', 'offer8'),
            1,
            0
          )
        ),
      # creating cols for amounts inside offers
      off_amount =
        ifelse(
          transaction == 1 &
            time <= (Duration + min(time)) &
            Offer_completed == 1 &
            Offer_num %in% c('offer1', 'offer2', 'offer4', 'offer5', 'offer6', 'offer7', 'offer9', 'offer10'),
          amount,
          ifelse(
            transaction == 1 &
              time <= (Duration + min(time)) &
              Offer_viewed == 1 &
              Offer_num %in% c('offer3', 'offer8'),
            amount,
            0
          )
        )
    )
  
  return(x7)
  
}

p1 <- offer_trans(period1)
p2 <- offer_trans(period2)
p3 <- offer_trans(period3)
p4 <- offer_trans(period4)
p5 <- offer_trans(period5)
p6 <- offer_trans(period6)

# joining the different periods
all_periods_df <-
  bind_rows(p1,p2,p3,p4,p5,p6)

# combining the transaction and amounts from all the periods
all_periods_df <- all_periods_df |>
  filter(Offer_num != 'no_offer') |>
  group_by(person_id, Offer_num) |>
  summarise(off_trans = sum(off_trans),
            off_amount = sum(off_amount))
# Creating a df with offer transactions in wide format
all_trans <- all_periods_df |>
  pivot_wider(id_cols = person_id,
              names_from = Offer_num,
              values_from = off_trans,
              names_prefix = 'trans_',
              values_fill = 0
  )
# Creating a df with offer amounts in wide format
all_ammounts <- all_periods_df |>
  pivot_wider(id_cols = person_id,
              names_from = Offer_num,
              values_from = off_amount,
              names_prefix = 'amount_',
              values_fill = 0
  )

# Joining the the transactions and amounts
trans_by_offer <- left_join(all_trans, all_ammounts, by='person_id')

# Checking for duplicates in trans_by_offer DF
dups <- trans_by_offer[duplicated(trans_by_offer) |
                         duplicated(trans_by_offer, fromLast = TRUE), ]
length(dups$person_id) ## no duplicates


################################################################################
# getting the offer summaries for each offer
################################################################################

offer_summaries <- transcript2 |>
  group_by(person_id, offer_num) |>
  summarise(
    off_rec = sum(offer_received),
    off_view = sum(offer_viewed),
    off_comp = sum(offer_completed),
    tot_reward = sum(reward_rec))

# dropping the transaction rows
offer_summaries <- offer_summaries[complete.cases(offer_summaries$offer_num), ]

# creating a wide format
offer_summaries <- offer_summaries |>
  pivot_wider(id_cols = person_id,
              names_from = offer_num,
              values_from = c(off_rec, off_view, off_comp, tot_reward),
              values_fill = 0)

################################################################################
# combining the offer summaries and transaction summaries for each offer
################################################################################
summaries_by_offer <- full_join(offer_summaries, trans_by_offer, by = 'person_id')
names(summaries_by_offer)
length(summaries_by_offer$person_id)

################################################################################
# getting overall offer and transaction totals
################################################################################
trans_summaries <- transcript2 |>
  group_by(person_id) |>
  summarise(tot_off_rec = sum(offer_received),
            tot_off_view = sum(offer_viewed),
            tot_off_comp = sum(offer_completed),
            tot_reward = sum(reward_rec),
            tot_trans = sum(transaction),
            tot_amount = sum(amount),
            ave_amount = round(mean(amount),digits=2),
            max_amount = max(amount))



################################################################################
# Getting the portfolio data in wide format
################################################################################
names(portfolio)
portfolio_wide <- portfolio |>
  pivot_wider(
    id_cols = offer_id,
    names_from = offer_num,
    values_from = c(reward_off, difficulty, duration, bogo, discount,
                    informational, email, mobile, social, web),
    values_fill = 0
  )
portfolio_wide_df <- t(data.frame(colSums(portfolio_wide[, -1])))
rownames(portfolio_wide_df) <- NULL

################################################################################
# Joining the dataframes
################################################################################

data_wide <- full_join(profile, trans_summaries, by = 'person_id')
data_wide <- full_join(data_wide, summaries_by_offer, by = 'person_id')
data_wide <- cbind(data_wide, portfolio_wide_df)

# Dropping the observations with age = 118
data_wide <- data_wide[data_wide$age!=118,]


# checking duplicates
duplicates <- data_wide[duplicated(data_wide) |
                          duplicated(data_wide, fromLast = TRUE), ]
length(duplicates$person_id)     # no duplicates

# replacing the na values for the 5 people who did not receive offers
data_wide[is.na(data_wide)] <- 0

# Adding variables for total transactions in and outside offers
colnames(data_wide)
data_wide <- data_wide |>
  mutate(
    tot_tran_in = rowSums(pick(
      trans_offer1, trans_offer2, trans_offer3, trans_offer4,
      trans_offer5, trans_offer6, trans_offer7, trans_offer8,
      trans_offer9, trans_offer10)),
    tot_tran_out = tot_trans - rowSums(pick(
      trans_offer1, trans_offer2, trans_offer3, trans_offer4,
      trans_offer5, trans_offer6, trans_offer7, trans_offer8,
      trans_offer9, trans_offer10)),
    tot_amount_in = rowSums(pick(
      amount_offer1, amount_offer2, amount_offer3, amount_offer4,
      amount_offer5, amount_offer6, amount_offer7, amount_offer8,
      amount_offer9, amount_offer10)),
    tot_amount_out = tot_amount - rowSums(pick(
      amount_offer1, amount_offer2, amount_offer3, amount_offer4,
      amount_offer5, amount_offer6, amount_offer7, amount_offer8,
      amount_offer9, amount_offer10))
    )
# checking totals
sum(data_wide$tot_amount)- sum(data_wide$tot_amount_in)-sum(data_wide$tot_amount_out)

# saving combined dataframe as csv
write.csv(data_wide, "data_wide2.csv", row.names = FALSE)

print(colnames(data_wide))
################################################################################
# putting data in long format
################################################################################

# combining the transaction and amounts from all the periods
all_periods_df2 <-
  bind_rows(p1,p2,p3,p4,p5,p6)

all_periods_df2 <- all_periods_df2 |>
  filter(Offer_num != 'no_offer') |>
  group_by(person_id, Offer_num) |>
  summarise(transactions = sum(off_trans),
            amount = sum(off_amount)) |>
  rename(offer_num = Offer_num)

offer_summaries2 <- transcript2 |>
  group_by(person_id, offer_num) |>
  summarise(
    off_rec = sum(offer_received),
    off_view = sum(offer_viewed),
    off_comp = sum(offer_completed),
    reward = sum(reward_rec))
# dropping the transaction rows
offer_summaries2 <- offer_summaries2[complete.cases(offer_summaries2$offer_num), ]

trans_plus_offers <- full_join(all_periods_df2, offer_summaries2,
                               by = c('person_id', 'offer_num'))

overall_summaries2 <- transcript2 |>
  group_by(person_id) |>
  summarise(off_rec = sum(offer_received),
            off_view = sum(offer_viewed),
            off_comp = sum(offer_completed),
            reward = sum(reward_rec),
            transactions = sum(transaction),
            amount = sum(amount)
  )
overall_summaries2$offer_num <- 'total'

trans_plus_offers2 <- rbind(trans_plus_offers, overall_summaries2)

# merging offer and transactions with profile and portfolio data

data_long <- full_join(trans_plus_offers2, profile, by = 'person_id')
data_long <- full_join(data_long, portfolio, by = 'offer_num')

# Dropping the observations with age = 118
data_long <- data_long[data_long$age!=118,] 


# saving combined dataframe as csv
write.csv(data_long, "data_long2.csv", row.names = FALSE)  