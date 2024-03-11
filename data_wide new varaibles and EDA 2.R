################################################################################
#--------------------------------DABI PROJECT ----------------------------------
################################################################################
library(dplyr)
library(lubridate)
library(readr)
library(corrplot)
library(ggplot2)

data_wide <- read_csv("data_wide4.csv")

################################################################################
## Adding New Variables to Wide Dataset
################################################################################
# 1. Engagement Rate Variables -------------------------------------------------
data_wide$offer_view_rate <- data_wide$tot_off_view / data_wide$tot_off_rec
summary(data_wide$offer_view_rate)
data_wide$offer_completion_rate <- data_wide$tot_off_comp / data_wide$tot_off_rec
summary(data_wide$offer_completion_rate)
data_wide$view_to_completion_rate <- data_wide$tot_off_comp / data_wide$tot_off_view
summary(data_wide$view_to_completion_rate)

# 2. Spending Behavior Variables ------------------------------------------------
# TOTAL AMOUNT SPENT == AVE SPEND & TENURE IS BEFORE TRANSCRIPT TAKES PLACE


# 3. Demographic Interaction Variables -----------------------------------------
# Categorizing age into groups
# data_wide$age_group <- cut(data_wide$age,
#                            breaks = c(-Inf, 25, 35, 45, 55, 65, Inf),
#                            labels = c("<=25", "26-35", "36-45", "46-55", "56-65", "66+"))
# summary(data_wide$age_group)

# Is it worthwhile first group 18-30 to make groups more even?
data_wide$age_group <- cut(data_wide$age,
                           breaks = c(-Inf, 30, 40, 50, 60, 70, Inf),
                           labels = c("<=30", "31-40", "41-50", "51-60", "61-70", "71+"))
summary(data_wide$age_group)

# Categorizing income into brackets --------------------------------------------
# data_wide$income_bracket <- cut(data_wide$income,
#                                 breaks = c(-Inf, 32000, 64000, 96000, Inf),
#                                 labels = c("Low", "Medium", "High", "Very High"))
# summary(data_wide$income_bracket)

# First group only has 500 in
# data_wide$income_bracket <- cut(data_wide$income,
#                                 breaks = c(-Inf, 50000, 70000, 90000, Inf),
#                                 labels = c("Low", "Medium", "High", "Very High"))
# summary(data_wide$income_bracket)
# 
data_wide$income_bracket <- cut(data_wide$income,
                                breaks = c(-Inf, 40000, 50000, 60000, 70000, 80000, Inf),
                                labels = c("30 - 40k", "40-50k","50-60k", "60-70k", "70-80k", "80k+"))
summary(data_wide$income_bracket)

# 4. Customer Loyalty Variables ------------------------------------------------
# Convert membership_start to tenure in years 
#data_wide$membership_duration_years <- as.numeric(difftime(Sys.Date(), data_wide$membership_start, units="days")) / 365.25
#summary(data_wide$membership_duration_years)


# 6. Offer-Type Preference  ----------------------------------------------------
#bogo
data_wide$bogo_view_rate<-data_wide$bogo_view/data_wide$bogo_rec
data_wide$bogo_response_rate<-data_wide$bogo_comp/data_wide$bogo_rec

#data_wide$bogo_view_rate<-base::ifelse(data_wide$bogo_view_rate==Inf, NA, data_wide$bogo_view_rate)
#data_wide$bogo_response_rate<-base::ifelse(data_wide$bogo_response_rate==Inf, NA, data_wide$bogo_response_rate)

summary(data_wide$bogo_view_rate)
summary(data_wide$bogo_response_rate)

# Discount offers
data_wide$disc_view_rate<-data_wide$disc_view/data_wide$disc_rec
data_wide$disc_response_rate<-data_wide$disc_comp/data_wide$disc_rec

#data_wide$disc_view_rate<-base::ifelse(data_wide$disc_view_rate==Inf, NA, data_wide$disc_view_rate)
#data_wide$disc_response_rate<-base::ifelse(data_wide$disc_response_rate==Inf, NA, data_wide$disc_response_rate)

summary(data_wide$disc_view_rate)
summary(data_wide$disc_response_rate)

# # Informational offers
data_wide$info_view_rate<-data_wide$info_view/data_wide$info_rec

#data_wide$info_view_rate<-base::ifelse(data_wide$info_view_rate==Inf, NA, data_wide$info_view_rate)
summary(data_wide$info_view_rate)
# 7. Channel Effectiveness -----------------------------------------------------
# channels <- c("email", "mobile", "social", "web")
# 
# for(channel in channels) {
#   channel_cols <- grep(paste0(channel, "_offer"), names(data_wide), value = TRUE)
#   comp_cols <- grep("off_comp_offer", names(data_wide), value = TRUE)
#   
#   total_channel_used <- rowSums(data_wide[, channel_cols], na.rm = TRUE)
#   total_completed <- rowSums(data_wide[, comp_cols], na.rm = TRUE)
#   
#   # Calculating effectiveness for each channel
#   data_wide[[paste0(channel, "_comp_rate")]] <- total_completed / total_channel_used
# }
data_wide$mob_view_rate<- data_wide$mob_view/data_wide$mob_rec
data_wide$mob_comp_rate<- data_wide$mob_comp/data_wide$mob_rec

summary(data_wide$mob_view_rate)
summary(data_wide$mob_comp_rate)

data_wide$web_view_rate<- data_wide$web_view/data_wide$web_rec
data_wide$web_comp_rate<- data_wide$web_comp/data_wide$web_rec

summary(data_wide$web_view_rate)
summary(data_wide$web_comp_rate)



data_wide$social_view_rate<- data_wide$social_view/data_wide$social_rec
data_wide$social_comp_rate<- data_wide$social_comp/data_wide$social_rec

summary(data_wide$social_view_rate)
summary(data_wide$social_comp_rate)
# 8. Promotion Interaction Rate ------------------------------------------------
data_wide$promotion_interaction_rate <- (data_wide$tot_off_view + data_wide$tot_off_comp) / data_wide$tot_off_rec
summary(data_wide$promotion_interaction_rate)
# 9. Promotion Conversion Rate -------------------------------------------------
#data_wide$promotion_conversion_rate <- with(data_wide, ifelse(tot_off_view > 0, tot_off_comp / tot_off_view, 0))
#summary(data_wide$promotion_conversion_rate)
#10 Percentage of reward cashed
data_wide$reward_rec_rate<-data_wide$tot_reward_rec/data_wide$reward_off
summary(data_wide$reward_rec_rate)

## EDA #########################################################################
# Engagement Rate Variables Visualization --------------------------------------
# Offer View Rate
ggplot(data_wide, aes(x = offer_view_rate)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  ggtitle("Offer View Rate Distribution")

# Offer Completion Rate
ggplot(data_wide, aes(x = offer_completion_rate)) + 
  geom_histogram(bins = 30, fill = "green", color = "black") +
  ggtitle("Offer Completion Rate Distribution")

# View to Completion Rate
ggplot(data_wide, aes(x = view_to_completion_rate)) + 
  geom_histogram(bins = 30, fill = "red", color = "black") +
  ggtitle("View to Completion Rate Distribution")

# Spending Behavior Visualization ----------------------------------------------
# Average Spend per Transaction
ggplot(data_wide, aes(x = average_spend_per_transaction)) + 
  geom_histogram(bins = 30, fill = "coral", color = "black") +
  ggtitle("Average Spend per Transaction")

# Spend per Day
ggplot(data_wide, aes(x = spend_per_day)) + 
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  ggtitle("Spend per Day")

#  Demographic Interaction Visualization ---------------------------------------
# Age Group
ggplot(data_wide, aes(x = age_group)) + 
  geom_bar(fill = "skyblue") +
  ggtitle("Customer Distribution by Age Group")

# Income Bracket
ggplot(data_wide, aes(x = income_bracket)) + 
  geom_bar(fill = "orange") +
  ggtitle("Customer Distribution by Income Bracket")

# Customer Loyalty Visualization -----------------------------------------------
# Membership Duration in Years
ggplot(data_wide, aes(x = membership_duration_years)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  ggtitle("Membership Duration Distribution")

# Composite Engagement Score Visualization -------------------------------------
# Composite Engagement Score
ggplot(data_wide, aes(x = composite_engagement_score)) + 
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  ggtitle("Composite Engagement Score Distribution")


# Offer-Type Preference Visualization ------------------------------------------
offer_type_rates <- data.frame(
  OfferType = c("BOGO", "Discount", "Informational"),
  ResponseRate = c(mean(data_wide$bogo_response_rate, na.rm = TRUE), 
                   mean(data_wide$discount_response_rate, na.rm = TRUE), 
                   mean(data_wide$informational_response_rate, na.rm = TRUE))
)

# Plotting
ggplot(offer_type_rates, aes(x = OfferType, y = ResponseRate)) + 
  geom_bar(stat = "identity", fill = "dodgerblue") +
  ggtitle("Response Rate by Offer Type")

# Channel Effectiveness Visualization ------------------------------------------
channel_effectiveness <- data.frame(
  Channel = c("Email", "Mobile", "Social", "Web"),
  CompRate = c(mean(data_wide$email_comp_rate, na.rm = TRUE), 
               mean(data_wide$mobile_comp_rate, na.rm = TRUE), 
               mean(data_wide$social_comp_rate, na.rm = TRUE), 
               mean(data_wide$web_comp_rate, na.rm = TRUE))
)

# Plotting
ggplot(channel_effectiveness, aes(x = Channel, y = CompRate)) + 
  geom_bar(stat = "identity", fill = "tomato") +
  ggtitle("Channel Effectiveness")


ggplot(data_wide, aes(x = promotion_interaction_rate)) +
  geom_histogram(bins = 30, fill = "blue") +
  labs(title = "Histogram of Promotion Interaction Rate", x = "Promotion Interaction Rate", y = "Frequency")

# Summary statistics
summary(data_wide$promotion_interaction_rate)


# Histogram of Promotion Conversion Rate
ggplot(data_wide, aes(x = promotion_conversion_rate)) +
  geom_histogram(bins = 30, fill = "green") +
  labs(title = "Histogram of Promotion Conversion Rate", x = "Promotion Conversion Rate", y = "Frequency")

# Summary statistics
summary(data_wide$promotion_conversion_rate)

# Boxplot for Promotion Interaction Rate
ggplot(data_wide, aes(y = promotion_interaction_rate)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot of Promotion Interaction Rate")

# Boxplot for Promotion Conversion Rate
ggplot(data_wide, aes(y = promotion_conversion_rate)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot of Promotion Conversion Rate")

# interaction between variables ------------------------------------------------
ggplot(data_wide, aes(x = promotion_interaction_rate, y = promotion_conversion_rate)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatterplot of Interaction Rate vs Conversion Rate", x = "Promotion Interaction Rate", y = "Promotion Conversion Rate")


# Corrplot ---------------------------------------------------------------------
correlation_columns <- c('age', 'income', 'tenure', 'composite_engagement_score', 'bogo_response_rate', 
                         'discount_response_rate', 'informational_response_rate', 'email_comp_rate', 
                         'mobile_comp_rate', 'social_comp_rate', 'web_comp_rate','promotion_interaction_rate',
                         'promotion_conversion_rate')

# Compute the correlation matrix
correlation_matrix <- cor(data_wide[, correlation_columns], use="complete.obs")

# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix", 
         tl.cex = 0.7, cl.cex = 0.7, cl.ratio = 0.3)

library(GGally)
ggpairs(data_wide[, sapply(data_wide, is.numeric)])

as.data.frame(colnames(data_wide))
as.data.frame(sapply(data_wide, class), )

for(i in colnames(data_wide)){
  print(length(unique(data_wide[,i])))
}

# give same result. Possibly only use one in final analysis
ggplot(data_wide, aes(tenure, membership_duration_years))+
  geom_point()

# what is the difference in calculation?
ggplot(data_wide, aes(average_spend_per_transaction, ave_amount))+
  geom_point()


# histograms of percentage of reward claimed
ggplot(data=data_wide, aes(perc_reward_cashed))+
  geom_histogram()

ggplot(data=data_wide, aes(perc_reward_cashed))+
  geom_histogram()+
  facet_wrap(vars(gender))

ggplot(data=data_wide, aes(perc_reward_cashed))+
  geom_histogram()+
  facet_wrap(vars(age_group))

ggplot(data=data_wide, aes(perc_reward_cashed))+
  geom_histogram()+
  facet_wrap(vars(income_bracket))

# histograms of percentage of difficulty claimed
ggplot(data=data_wide, aes(perc_difficulty_cashed))+
  geom_histogram()

ggplot(data=data_wide, aes(perc_difficulty_cashed))+
  geom_histogram()+
  facet_wrap(vars(gender))

ggplot(data=data_wide, aes(perc_difficulty_cashed))+
  geom_histogram()+
  facet_wrap(vars(age_group))

ggplot(data=data_wide, aes(perc_difficulty_cashed))+
  geom_histogram()+
  facet_wrap(vars(income_bracket))

# channels of offers completed vs age gender and income
ggplot(data_wide, aes(email_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(income_bracket))

ggplot(data_wide, aes(mobile_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(income_bracket))

ggplot(data_wide, aes(social_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(income_bracket))

ggplot(data_wide, aes(web_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(income_bracket))

ggplot(data_wide, aes(email_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(age_group))

ggplot(data_wide, aes(mobile_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(age_group))

ggplot(data_wide, aes(social_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(age_group))

ggplot(data_wide, aes(web_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(age_group))

ggplot(data_wide, aes(email_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(gender))

ggplot(data_wide, aes(mobile_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(gender))

ggplot(data_wide, aes(social_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(gender))

ggplot(data_wide, aes(web_comp_rate)) +
  geom_bar() +
  facet_wrap(vars(gender))

##########################
# rfm added to wide_data 
##########################

rfm_table<- read.csv("rfm_table.csv")
colnames(rfm_table)
rfm_table<-rfm_table %>% 
  select(person_id, r_score,f_score, m_score, rfm_score, rfm_string)


data_wide3<-merge(data_wide, rfm_table, by="person_id")
summary(data_wide3$r_score)
summary(data_wide3$f_score)
summary(data_wide3$m_score)
summary(data_wide3$rfm_score)
summary(data_wide3$rfm_string)

data_wide3$rfm_string<- as.character(data_wide3$rfm_string)

data_wide3<- data_wide3 %>% select(-...1,-X)
write.csv(data_wide3, "data_wide_temp.csv", row.names = FALSE) 
colnames(data_wide3)
