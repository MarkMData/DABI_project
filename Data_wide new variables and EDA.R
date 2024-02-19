################################################################################
#--------------------------------DABI PROJECT ----------------------------------
################################################################################
library(dplyr)
library(lubridate)
library(readr)
library(corrplot)
library(ggplot2)

data_wide <- read_csv("data_wide2.csv")

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
data_wide$average_spend_per_transaction <- data_wide$tot_amount / data_wide$tot_trans
summary(data_wide$average_spend_per_transaction)
data_wide$spend_per_day <- data_wide$tot_amount / data_wide$tenure
summary(data_wide$spend_per_day)

# 3. Demographic Interaction Variables -----------------------------------------
# Categorizing age into groups
data_wide$age_group <- cut(data_wide$age,
                           breaks = c(-Inf, 25, 35, 45, 55, 65, Inf),
                           labels = c("<=25", "26-35", "36-45", "46-55", "56-65", "66+"))
summary(data_wide$age_group)

# Categorizing income into brackets --------------------------------------------
data_wide$income_bracket <- cut(data_wide$income,
                                breaks = c(-Inf, 32000, 64000, 96000, Inf),
                                labels = c("Low", "Medium", "High", "Very High"))
summary(data_wide$income_bracket)

# 4. Customer Loyalty Variables ------------------------------------------------
# Convert membership_start to tenure in years 
data_wide$membership_duration_years <- as.numeric(difftime(Sys.Date(), data_wide$membership_start, units="days")) / 365.25
summary(data_wide$membership_duration_years)

# 5. Composite Engagement Score ------------------------------------------------
data_wide$composite_engagement_score <- (data_wide$offer_view_rate + data_wide$offer_completion_rate + data_wide$average_spend_per_transaction) / 3
summary(data_wide$composite_engagement_score)

# 6. Offer-Type Preference  ----------------------------------------------------
# Initialize columns for response rates
data_wide$bogo_response_rate <- 0
data_wide$discount_response_rate <- 0
data_wide$informational_response_rate <- 0

# # Bogo offers
bogo_rec_cols <- grep("off_rec_offer[0-9]+", names(data_wide), value = TRUE)
bogo_comp_cols <- grep("off_comp_offer[0-9]+", names(data_wide), value = TRUE)

data_wide$bogo_response_rate <- rowSums(data_wide[, bogo_comp_cols], na.rm = TRUE) / rowSums(data_wide[, bogo_rec_cols], na.rm = TRUE)

# Discount offers
discount_rec_cols <- grep("off_rec_offer[0-9]+", names(data_wide), value = TRUE)
discount_comp_cols <- grep("off_comp_offer[0-9]+", names(data_wide), value = TRUE)

data_wide$discount_response_rate <- rowSums(data_wide[, discount_comp_cols], na.rm = TRUE) / rowSums(data_wide[, discount_rec_cols], na.rm = TRUE)

# Informational offers
informational_rec_cols <- grep("off_rec_offer[0-9]+", names(data_wide), value = TRUE)
informational_comp_cols <- grep("off_comp_offer[0-9]+", names(data_wide), value = TRUE) # Note: Informational offers may not have a 'completed' status in some contexts

data_wide$informational_response_rate <- rowSums(data_wide[, informational_comp_cols], na.rm = TRUE) / rowSums(data_wide[, informational_rec_cols], na.rm = TRUE)

# 7. Channel Effectiveness -----------------------------------------------------
channels <- c("email", "mobile", "social", "web")

for(channel in channels) {
  channel_cols <- grep(paste0(channel, "_offer"), names(data_wide), value = TRUE)
  comp_cols <- grep("off_comp_offer", names(data_wide), value = TRUE)
  
  total_channel_used <- rowSums(data_wide[, channel_cols], na.rm = TRUE)
  total_completed <- rowSums(data_wide[, comp_cols], na.rm = TRUE)
  
  # Calculating effectiveness for each channel
  data_wide[[paste0(channel, "_comp_rate")]] <- total_completed / total_channel_used
}

# 8. Promotion Interaction Rate ------------------------------------------------
data_wide$promotion_interaction_rate <- (data_wide$tot_off_view + data_wide$tot_off_comp) / data_wide$tot_off_rec

# 9. Promotion Conversion Rate -------------------------------------------------
data_wide$promotion_conversion_rate <- with(data_wide, ifelse(tot_off_view > 0, tot_off_comp / tot_off_view, 0))

#10 Percentage of reward cashed

summary(data_wide$perc_reward_cashed)
sum(data_wide$perc_reward_cashed==Inf, na.rm=TRUE)
data_wide %>% filter(perc_reward_cashed>1) %>% select(perc_reward_cashed)

data_wide<-data_wide %>% mutate(perc_reward_cashed=tot_reward/(off_rec_offer1*reward_off_offer1+
                                                                  off_rec_offer2*reward_off_offer2+off_rec_offer3*reward_off_offer3+
                                                                  off_rec_offer4*reward_off_offer4+off_rec_offer5*reward_off_offer5+
                                                                  off_rec_offer6*reward_off_offer6+off_rec_offer7*reward_off_offer7+
                                                                  off_rec_offer8*reward_off_offer8+off_rec_offer9*reward_off_offer9+
                                                                  off_rec_offer10*reward_off_offer10))

summary(data_wide$perc_reward_cashed)

#11 percentage of difficulty of offer taken
data_wide<-data_wide %>% mutate(perc_difficulty_cashed=(off_comp_offer1*difficulty_offer1+
                                                          off_comp_offer2*difficulty_offer2+off_comp_offer3*difficulty_offer3+
                                                          off_comp_offer4*difficulty_offer4+off_comp_offer5*difficulty_offer5+
                                                          off_comp_offer6*difficulty_offer6+off_comp_offer7*difficulty_offer7+
                                                          off_comp_offer8*difficulty_offer8+off_comp_offer9*difficulty_offer9+
                                                          off_comp_offer10*difficulty_offer10)/(off_rec_offer1*difficulty_offer1+
                                                                 off_rec_offer2*difficulty_offer2+off_rec_offer3*difficulty_offer3+
                                                                 off_rec_offer4*difficulty_offer4+off_rec_offer5*difficulty_offer5+
                                                                 off_rec_offer6*difficulty_offer6+off_rec_offer7*difficulty_offer7+
                                                                 off_rec_offer8*difficulty_offer8+off_rec_offer9*difficulty_offer9+
                                                                 off_rec_offer10*difficulty_offer10))

summary(data_wide$perc_difficulty_cashed)

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
