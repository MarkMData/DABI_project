################################################################################
## K-prototype for wide_data5
################################################################################
## Libraries
install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)
library(clustMixType)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)

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
clust_colmap = c("#f7286d","#1faae0","#ffbf1f")

## CHECK THE DATA
str(data_wide5)
skimr::skim(data_wide5)

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


## Start clustering ############################################################

selected_vars <- c("tot_amount", "gender", "income","age", "reward_off", "offer_view_rate",
                   "tot_reward_rec", "reward_rec_rate","tot_off_comp", "tenure", "tot_trans",
                   "mob_comp_rate","web_comp_rate","social_comp_rate" )

data_selected <- data_wide5[, selected_vars]

skimr::skim(data_selected)

## checking for NAs in selected data -------------------------------------------
bar_missing <- function(x){
  require(reshape2)
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2)) +
    geom_bar(aes(y=(..count..),fill=value),alpha=0.7)+
    scale_fill_manual(values=c("skyblue","red"),
                      name = "",
                      labels = c("Available","Missing"))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Observations")+coord_flip()
}

bar_missing(data_selected)

## impute missing data using mice() function -----------------------------------
missing_data <- mice(data_selected)

# Generate multiple imputed datasets
imputed_data <- complete(missing_data)


## DATA EXPLORATION ############################################################
## CORRPLOT of numerical vars --------------------------------------------------
numeric_vars <- sapply(imputed_data, is.numeric)
numeric <- imputed_data[, numeric_vars]

# Calculating the correlation matrix
cor_matrix <- cor(numeric, use = "complete.obs") 

corrplot(cor_matrix, method = "circle",type = "lower", 
         tl.col = "black", tl.srt = 45, 
         tl.cex = 0.5, 
         diag = FALSE, 
         cl.lim = c(-1, 1),
         cl.cex = 0.75, 
         addCoef.col = "black") 

## Graphs ----------------------------------------------------------------------
imputed_data %>% gather(c(tot_amount,tot_off_comp, income, tenure, tot_trans),
              key="Parameter",
              value="Value")%>%
  ggplot(aes(x=Value,fill=gender))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey"))+
  my_theme()

imputed_data %>%
  pivot_longer(cols = c(tot_amount,tot_off_comp, income, tenure, tot_trans ),
               names_to = "Parameter",
               values_to = "Value") %>%
  ggplot(aes(x=Value, fill=gender)) +
  geom_density(alpha=0.5) +
  ggtitle("Hypertension") +
  facet_wrap(~Parameter, ncol=2, scales="free") +
  scale_fill_manual(values=c("blue", "red", "grey")) +
  theme_minimal() 


ggplot(imputed_data, aes(x=gender, y=income, fill=gender)) +
  geom_boxplot() +
  scale_fill_manual(values=c("blue", "red", "grey")) +
  theme_minimal() 

## standardise the data for clustering -----------------------------------------
# Identify numeric cols
data_standardized <- imputed_data %>%
  mutate(across(where(is.numeric), scale))

# View the first few rows of the standardized data
head(data_standardized)

## find k (option 1) -----------------------------------------------------------
x_mat <- data_standardized[,-1]
skimr::skim(x_mat)
Es <- numeric(10)

for(i in 1:10){
  kpres <- kproto(x_mat, 
                  k = i, nstart = 5, 
                  lambda = lambdaest(x_mat),
                  verbose = FALSE)
  Es[i] <- kpres$tot.withinss}


tibble(Cluster = c(1:10), Es = Es) %>% 
  ggplot(aes(x = Cluster, y = Es)) + 
  geom_point(size = 3, 
             col ="red3") +
  geom_path() + 
  geom_vline(xintercept = 3, 
             linetype = 2)+
  scale_x_continuous(breaks = c(1:10))

# k-opt
k_opt <- validation_kproto(method = "ptbiserial", data = x_mat, k = 2:10, nstart = 5)

tibble(Cluster = c(2:10), 
       Metric = as.vector(k_opt$indices)) %>% 
  ggplot(aes(x = Cluster, 
             y = Metric)) + 
  geom_point(size = 3, 
             col ="red3") +
  geom_path() + 
  geom_vline(xintercept = 3, 
             linetype = 2)+
  scale_x_continuous(breaks = c(2:10))


## find k (option 2) -----------------------------------------------------------
set.seed(7)

total_withinss <- c()

for (i in 1:8) {
  kproto <- clustMixType::kproto(x_mat,
                                 k = i,
                                 nstart = 25)
  total_withinss[i] <- kproto$tot.withinss
}

tibble(k = 1:length(total_withinss),
       total_error = total_withinss
) %>%
  ggplot(aes(x = k,
             y = total_error)
  ) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  labs(x = "Number of Clusters",
       y = "tot.withinss") +
  geom_text(x = 3,
            y = total_withinss[3],
            label = "ELBOW",
            alpha = 0.5,
            color = "blue",
            size = 5)

## CLUSTERING ##################################################################
kpres_3 = kproto(x = x_mat,
                 k = 3,
                 lambda = lambdaest(x_mat))
str(valid_df)
## Investigate the cluster (prototype) #########################################

valid_df = data_wide5 %>% mutate(Cluster = as.factor( kpres_3$cluster))

valid_df %>% gather(c(age,income, tenure, tot_amount,tot_reward_rec),
                key="Parameter",
                value="Value")%>%
  ggplot(aes(x=Value,fill=Cluster))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey"))+
  my_theme()

valid_df %>%gather(age,income, tenure, tot_amount,tot_reward_rec, 
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster, y=Value, fill = Cluster))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap )

valid_df %>%gather(age,income, tenure, tot_amount, bogo_comp, disc_comp,  
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster, y=Value, fill = Cluster))+
  geom_boxplot(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap )

valid_df %>% ggplot(aes(x=age, y=tot_amount)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster, col = Cluster, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap) +
  scale_color_manual(values = clust_colmap)

valid_df %>% ggplot(aes(x=income, y=tot_trans)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster, col = Cluster, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap) +
  scale_color_manual(values = clust_colmap)

# Scatter plot for two variables with clusters colored
ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster)) +
  geom_point(alpha = 0.5) +
  theme_minimal()

# Box plot for comparing a variable's distribution across clusters
ggplot(valid_df, aes(x = Cluster, y = tot_amount, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal()

# Density plot for a single variable
ggplot(valid_df, aes(x = tot_amount, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

cor_res <- cor(select(valid_df, where(is.numeric)))
corrplot::corrplot(cor_res, method = "circle")

table <- valid_df %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

view(table)

ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Age vs Total Amount Spent by Cluster", x = "Age", y = "Total Amount Spent")

ggplot(valid_df, aes(x = Cluster, y = income, fill = Cluster)) +
  geom_violin(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income") +
  scale_fill_manual(values = c("blue", "red", "grey"))

ggplot(valid_df, aes(x = tenure, y = tot_reward_rec, color = Cluster)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tenure vs Total Rewards Received by Cluster", x = "Tenure", y = "Total Rewards Received")


# Recency Score Distribution
ggplot(valid_df, aes(x = Cluster, y = r_score, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Recency Score Distribution by Cluster", x = "Cluster", y = "Recency Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Frequency Score Distribution
ggplot(valid_df, aes(x = Cluster, y = f_score, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Frequency Score Distribution by Cluster", x = "Cluster", y = "Frequency Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Monetary Score Distribution
ggplot(valid_df, aes(x = Cluster, y = m_score, fill = Cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monetary Score Distribution by Cluster", x = "Cluster", y = "Monetary Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Frequency vs. Monetary Score Scatter Plot
ggplot(valid_df, aes(x = f_score, y = m_score, color = Cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Frequency vs. Monetary Score by Cluster", x = "Frequency Score", y = "Monetary Score") +
  scale_color_manual(values = c("blue", "red", "grey"))



cor_res <- cor(select(valid_df[valid_df$Cluster == "1",], c(r_score, f_score, m_score)))

# Melt the correlation matrix for heatmap plotting
melted_cor <- melt(cor_res)

# Heatmap Plot
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of RFM Scores", x = "", y = "")


# 1. Summarize Offer Response Data by Cluster
offer_response_summary <- valid_df %>%
  group_by(Cluster) %>%
  summarise(
    Avg_BOGO_Completion = mean(bogo_comp, na.rm = TRUE),
    Avg_Discount_Completion = mean(disc_comp, na.rm = TRUE),
    Avg_Total_Completion = mean(tot_off_comp, na.rm = TRUE)
  )

# BOGO Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster, y = Avg_BOGO_Completion, fill = Cluster)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average BOGO Offer Completion by Cluster", x = "Cluster", y = "Average BOGO Completion")

# Discount Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster, y = Avg_Discount_Completion, fill = Cluster)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Discount Offer Completion by Cluster", x = "Cluster", y = "Average Discount Completion")

melted_data <- melt(offer_response_summary, id.vars = "Cluster")

ggplot(melted_data, aes(x = variable, y = Cluster, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Offer Completion Rate Heatmap by Cluster", x = "Offer Type", y = "Cluster")


chisq.test(table(valid_df$Cluster, valid_df$offer_type1)) 
# significant association between the clusters and how they respond to offer type 1


