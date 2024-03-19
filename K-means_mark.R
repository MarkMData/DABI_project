################################################################################
## K-prototype for wide_data5
################################################################################
## Libraries
library(ggcorrplot)
library(tidyverse)
library(clustMixType)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(caret)
library(mice)
library(fdm2id)
library(scales)
library(flexclust)
library(skimr)

data_wide5 <- read_csv("data_wide5.csv")

my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 10),
      panel.grid.major = element_line(color = "gray"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#f7fdff"),
      strip.background = element_rect(fill = "#001d60", color = "#00113a", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey5", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())
clust_colmap3 = c("#f7286d","#1faae0","#ffbf1f")
clust_colmap4 = c("#f7286d","#1faae0","#ffbf1f","#98FB98")

## remove duplicate cols -------------------------------------------------------
data_wide5 <- data_wide5 %>% 
  select(-matches("\\.\\.\\.1"))

### Categorical vars ###########################################################
## convert character vars to factor --------------------------------------------
character_vars <- sapply(data_wide5, is.character)
char_vars_names <- names(character_vars[character_vars == TRUE])
char_vars_names

# Convert character variables to factors
data_wide5$gender <- as.factor(data_wide5$gender)
data_wide5$age_group <- as.factor(data_wide5$age_group)
data_wide5$income_bracket <- as.factor(data_wide5$income_bracket)
data_wide5$offer_type1 <- as.factor(data_wide5$offer_type1)
data_wide5$offer_type2 <- as.factor(data_wide5$offer_type2)
data_wide5$offer_type3 <- as.factor(data_wide5$offer_type3)
data_wide5$offer_type4 <- as.factor(data_wide5$offer_type4)
data_wide5$offer_type5 <- as.factor(data_wide5$offer_type5)
data_wide5$offer_type6 <- as.factor(data_wide5$offer_type6)

################################################################################
## decide on clustering variables
################################################################################

# Imputing missing data for rates
missing_data <- mice(data_wide5)
imputed_data <- complete(missing_data)
skim(imputed_data)

# splitting data while presevering the distribution of variable tot_amount
set.seed(123)
index <- createDataPartition(imputed_data$tot_amount, p=0.7, list=FALSE)
train <- imputed_data[index,]
test <- imputed_data[-index,]
data_train <- data_wide5[index,]
data_test <- data_wide5[-index,]

# Selecting variables that are not influenced by random chance such
# the number of offers people receive

selected_vars <- c("income","age", "tenure",
                   "ave_amount", "offer_view_rate",
                   "tot_trans","bogo_response_rate", "disc_response_rate")

train_clust <- train[,selected_vars]
test_clust <- test[, selected_vars]

train_stand <- train_clust %>%
  mutate(across(where(is.numeric), scale))
test_stand <- test_clust %>%
  mutate(across(where(is.numeric), scale))

################################################################################
## CLUSTERING
################################################################################

kmeans4 <- KMEANS(train_stand, k = 4,iter.max = 100, nstart = 100)
test_clusters <- predict(kmeans4, newdata = test_stand)

train_df <- data_train %>% mutate(Cluster4 = as.factor( kmeans4$cluster))
test_df <- data_test |> mutate(Cluster4 = as.factor(test_clusters))

################################################################################
# Looking at 4 clusters training set
################################################################################

# gender bar chart
ggplot(train_df,aes(Cluster4))+
  geom_bar(aes(Cluster4, fill=gender), alpha = 0.5)+
  geom_text(stat = 'count',
            aes(label=percent(after_stat(count)/nrow(train_df))),
            size = 4, 
            fontface = "bold")+
  labs(title = "Training set", x = "", y = "") 

####  RMF ###################################### 
train_df %>%gather(c(
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
ggplot(train_df, aes(x = f_score, y = m_score, color = Cluster4)) +
  geom_point(alpha = 0.6,position = "jitter") +
  theme_minimal() +
  labs(title = "Training set", x = "Frequency Score", y = "Monetary Score") +
  scale_fill_manual(values=clust_colmap4)


####### demographics, overall summaries ###########
# violins
train_df %>%gather(c(
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
bogo <- train_df %>% filter(bogo_rec!=0)

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

disc <- train_df %>% filter(disc_rec!=0) 
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

info <- train_df %>% filter(info_rec!=0)

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
web <- train_df %>% filter(web_rec!=0)
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
mob <- train_df %>% filter(mob_rec!=0)
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
soc <- train_df %>% filter(social_rec!=0)
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

four_clust_summary <- train_df %>%
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


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Looking at cluster patterns in test data
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# gender bar chart
ggplot(test_df,aes(Cluster4))+
  geom_bar(aes(Cluster4, fill=gender), alpha = 0.5)+
  geom_text(stat = 'count',
            aes(label=percent(after_stat(count)/nrow(test_df))),
            size = 4, 
            fontface = "bold")+
  labs(title = "Test set", x = "", y = "")

####  RMF ###################################### 
test_df %>%gather(c(
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
ggplot(test_df, aes(x = f_score, y = m_score, color = Cluster4)) +
  geom_point(alpha = 0.6,position = "jitter") +
  theme_minimal() +
  labs(title = "Test set", x = "Frequency Score", y = "Monetary Score") +
  scale_fill_manual(values=clust_colmap4)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

