################################################################################
## K-prototype for wide_data5 log data
################################################################################
## Libraries

library(ggcorrplot)
library(tidyverse)
library(clustMixType)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(cluster)
library(mice)

data_wide5 <- read_csv("data_wide5.1.csv")

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
clust_colmap2 = c("#f7286d","#1faae0","#ffbf1f", "#66cc00")
clust_colmap3 = c("#f7286d","#1faae0","#ffbf1f", "#66cc00","#666666")

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
colnames(data_wide5)
selected_vars <- c("log_tot_amount", "gender", "log_income","age", "reward_off", "offer_view_rate",
                   "tot_reward_rec", "reward_rec_rate","tot_off_comp", "log_tenure", "log_tot_trans",
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
imputed_data
## Graphs ----------------------------------------------------------------------
imputed_data %>% gather(c(log_tot_amount,tot_off_comp, log_income, log_tenure, log_tot_trans),
                        key="Parameter",
                        value="Value")%>%
  ggplot(aes(x=Value,fill=gender))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey"))+
  my_theme()

imputed_data %>%
  pivot_longer(cols = c(log_tot_amount,tot_off_comp, log_income, log_tenure, log_tot_trans ),
               names_to = "Parameter",
               values_to = "Value") %>%
  ggplot(aes(x=Value, fill=gender)) +
  geom_density(alpha=0.5) +
  ggtitle("Hypertension") +
  facet_wrap(~Parameter, ncol=2, scales="free") +
  scale_fill_manual(values=c("blue", "red", "grey")) +
  theme_minimal() 


ggplot(imputed_data, aes(x=gender, y=log_income, fill=gender)) +
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
k_opt <- validation_kproto(data = x_mat, k = 2:6, nstart = 5)
k_opt$k_opt
k_opt$indices
tibble(Cluster = c(2:6), 
       Metric = as.vector(k_opt$indices)) %>% 
  ggplot(aes(x = Cluster, 
             y = Metric)) + 
  geom_point(size = 3, 
             col ="red3") +
  geom_path() + 
  geom_vline(xintercept = 2, 
             linetype = 2)+
  scale_x_continuous(breaks = c(2:6))


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
kpres_2 = kproto(x = x_mat,
                 k = 2,
                 lambda = lambdaest(x_mat), nstart=10)

kpres_3 = kproto(x = x_mat,
                 k = 3,
                 lambda = lambdaest(x_mat), nstart=10)

kpres_4 = kproto(x = x_mat,
                 k = 4,
                 lambda = lambdaest(x_mat), nstart=10)


kpres_5 = kproto(x = x_mat,
                 k = 5,
                 lambda = lambdaest(x_mat), nstart=10)


## Investigate the cluster (prototype) #########################################
valid_df = data_wide5 %>% mutate(Cluster2 = as.factor( kpres_2$cluster),Cluster3 = as.factor( kpres_3$cluster),
                                 Cluster4 = as.factor( kpres_4$cluster),Cluster5 = as.factor( kpres_5$cluster))


#########################
##investigate 3 clusters
########################
valid_df %>% gather(c(age,income, tenure, tot_amount,tot_reward_rec),
                    key="Parameter",
                    value="Value")%>%
  ggplot(aes(x=Value,fill=Cluster3))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey"))+
  my_theme()

valid_df %>%gather(age,income, tenure, tot_amount,tot_reward_rec, 
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster3, y=Value, fill = Cluster3))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap )

valid_df %>%gather(age,income, tenure, tot_amount, bogo_comp, disc_comp,  
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster3, y=Value, fill = Cluster3))+
  geom_boxplot(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap )

valid_df %>% ggplot(aes(x=age, y=tot_amount)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster3, col = Cluster3, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap) +
  scale_color_manual(values = clust_colmap)

valid_df %>% ggplot(aes(x=income, y=tot_trans)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster3, col = Cluster3, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap) +
  scale_color_manual(values = clust_colmap)

# Scatter plot for two variables with clusters colored
ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster3)) +
  geom_point(alpha = 0.5) +
  theme_minimal()

# Box plot for comparing a variable's distribution across clusters
ggplot(valid_df, aes(x = Cluster3, y = tot_amount, fill = Cluster3)) +
  geom_boxplot() +
  theme_minimal()

# Density plot for a single variable
ggplot(valid_df, aes(x = tot_amount, fill = Cluster3)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x = ave_amount, fill = Cluster3)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x=Cluster3,y = ave_amount, fill = Cluster3)) +
  geom_boxplot() +
  ylim(c(0,50))+
  theme_minimal()

cor_res <- cor(select(valid_df, where(is.numeric)))
corrplot::corrplot(cor_res, method = "circle")

table <- valid_df %>%
  group_by(Cluster3) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

view(table)

valid_df %>%
  group_by(Cluster3,gender) %>%
  count()

ggplot(valid_df, aes(Cluster3))+
  geom_bar(aes(fill=gender,y=after_stat(count/sum(count))))


ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster3)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Age vs Total Amount Spent by Cluster", x = "Age", y = "Total Amount Spent")

ggplot(valid_df, aes(x = Cluster3, y = income, fill = Cluster3)) +
  geom_violin(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income") +
  scale_fill_manual(values = c("blue", "red", "grey"))

ggplot(valid_df, aes(x = tenure, y = tot_reward_rec, color = Cluster3)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tenure vs Total Rewards Received by Cluster", x = "Tenure", y = "Total Rewards Received")


# Recency Score Distribution
ggplot(valid_df, aes(x = Cluster3, y = r_score, fill = Cluster3)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Recency Score Distribution by Cluster", x = "Cluster", y = "Recency Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Frequency Score Distribution
ggplot(valid_df, aes(x = Cluster3, y = f_score, fill = Cluster3)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Frequency Score Distribution by Cluster", x = "Cluster", y = "Frequency Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Monetary Score Distribution
ggplot(valid_df, aes(x = Cluster3, y = m_score, fill = Cluster3)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monetary Score Distribution by Cluster", x = "Cluster", y = "Monetary Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Frequency vs. Monetary Score Scatter Plot
ggplot(valid_df, aes(x = f_score, y = m_score, color = Cluster3)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Frequency vs. Monetary Score by Cluster", x = "Frequency Score", y = "Monetary Score") +
  scale_color_manual(values = c("blue", "red", "grey"))



cor_res <- cor(select(valid_df[valid_df$Cluster3 == "1",], c(r_score, f_score, m_score)))

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
  group_by(Cluster3) %>%
  summarise(
    Avg_BOGO_Completion = mean(bogo_comp, na.rm = TRUE),
    Avg_Discount_Completion = mean(disc_comp, na.rm = TRUE),
    Avg_Total_Completion = mean(tot_off_comp, na.rm = TRUE)
  )

# BOGO Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster3, y = Avg_BOGO_Completion, fill = Cluster3)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average BOGO Offer Completion by Cluster", x = "Cluster", y = "Average BOGO Completion")

# Discount Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster3, y = Avg_Discount_Completion, fill = Cluster3)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Discount Offer Completion by Cluster", x = "Cluster", y = "Average Discount Completion")

melted_data <- melt(offer_response_summary, id.vars = "Cluster3")

ggplot(melted_data, aes(x = variable, y = Cluster3, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Offer Completion Rate Heatmap by Cluster", x = "Offer Type", y = "Cluster")


chisq.test(table(valid_df$Cluster3, valid_df$offer_type1)) 
# significant association between the clusters and how they respond to offer type 1

valid_df %>% group_by(Cluster3) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))

v1<-valid_df %>% group_by(Cluster3, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1)) %>% print(n=Inf)
v2<-valid_df %>% group_by(Cluster3, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2)) %>% print(n=Inf)
v3<-valid_df %>% group_by(Cluster3, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3)) %>% print(n=Inf)
v4<-valid_df %>% group_by(Cluster3, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4)) %>% print(n=Inf)
v5<-valid_df %>% group_by(Cluster3, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5)) %>% print(n=Inf)
v6<-valid_df %>% group_by(Cluster3, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))

v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5]
total_offer_behaviour<- data.frame(v1$Cluster3, v1$offer_type1, v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5])
total_offer_behaviour
valid_df$offer_type4
bogo_clust1_period1<-valid_df %>% filter(Cluster3==1, offer_type1=="bogo")
disc_clust1_period1<-valid_df %>% filter(Cluster3==1, offer_type1=="discount")
none_clust1_period1<-valid_df %>% filter(Cluster3==1, offer_type1=="none")
info_clust1_period1<-valid_df %>% filter(Cluster3==1, offer_type1=="informational")

t.test(bogo_clust1_period1$log_tot_trans,none_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)



#########################
##investigate 4 clusters
########################
valid_df %>% gather(c(age,income, tenure, tot_amount,tot_reward_rec),
                    key="Parameter",
                    value="Value")%>%
  ggplot(aes(x=Value,fill=Cluster4))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey","green"))+
  my_theme()

valid_df %>%gather(age,income, tenure, tot_amount,tot_reward_rec, 
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap2 )

valid_df %>%gather(age,income, tenure, tot_amount, bogo_comp, disc_comp,  
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster4, y=Value, fill = Cluster4))+
  geom_boxplot(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap2 )

valid_df %>% ggplot(aes(x=age, y=tot_amount)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster4, col = Cluster4, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap2) +
  scale_color_manual(values = clust_colmap2)

valid_df %>% ggplot(aes(x=income, y=tot_trans)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster4, col = Cluster4, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap2) +
  scale_color_manual(values = clust_colmap2)

# Scatter plot for two variables with clusters colored
ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster4)) +
  geom_point(alpha = 0.5) +
  theme_minimal()

# Box plot for comparing a variable's distribution across clusters
ggplot(valid_df, aes(x = Cluster4, y = tot_amount, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal()

# Density plot for a single variable
ggplot(valid_df, aes(x = tot_amount, fill = Cluster4)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x = ave_amount, fill = Cluster4)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x=Cluster4,y = ave_amount, fill = Cluster4)) +
  geom_boxplot() +
  ylim(c(0,50))+
  theme_minimal()

cor_res <- cor(select(valid_df, where(is.numeric)))
corrplot::corrplot(cor_res, method = "circle")

table <- valid_df %>%
  group_by(Cluster4) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

view(table)

valid_df %>%
  group_by(Cluster4,gender) %>%
  count()

ggplot(valid_df, aes(Cluster4))+
  geom_bar(aes(fill=gender,y=after_stat(count/sum(count))))


ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster4)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Age vs Total Amount Spent by Cluster", x = "Age", y = "Total Amount Spent")

ggplot(valid_df, aes(x = Cluster4, y = income, fill = Cluster4)) +
  geom_violin(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income") +
  scale_fill_manual(values = c("blue", "red", "grey","green"))

ggplot(valid_df, aes(x = tenure, y = tot_reward_rec, color = Cluster4)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tenure vs Total Rewards Received by Cluster", x = "Tenure", y = "Total Rewards Received")


# Recency Score Distribution
ggplot(valid_df, aes(x = Cluster4, y = r_score, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Recency Score Distribution by Cluster", x = "Cluster", y = "Recency Score") +
  scale_fill_manual(values = c("blue", "red", "grey","green"))

# Frequency Score Distribution
ggplot(valid_df, aes(x = Cluster4, y = f_score, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Frequency Score Distribution by Cluster", x = "Cluster", y = "Frequency Score") +
  scale_fill_manual(values = c("blue", "red", "grey","green"))



# Monetary Score Distribution
ggplot(valid_df, aes(x = Cluster4, y = m_score, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monetary Score Distribution by Cluster", x = "Cluster", y = "Monetary Score") +
  scale_fill_manual(values = c("blue", "red", "grey","green"))

# Frequency vs. Monetary Score Scatter Plot
ggplot(valid_df, aes(x = f_score, y = m_score, color = Cluster4)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Frequency vs. Monetary Score by Cluster", x = "Frequency Score", y = "Monetary Score") +
  scale_color_manual(values = c("blue", "red", "grey","green"))



cor_res <- cor(select(valid_df[valid_df$Cluster4 == "1",], c(r_score, f_score, m_score)))

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
  group_by(Cluster4) %>%
  summarise(
    Avg_BOGO_Completion = mean(bogo_comp, na.rm = TRUE),
    Avg_Discount_Completion = mean(disc_comp, na.rm = TRUE),
    Avg_Total_Completion = mean(tot_off_comp, na.rm = TRUE)
  )

# BOGO Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster4, y = Avg_BOGO_Completion, fill = Cluster4)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average BOGO Offer Completion by Cluster", x = "Cluster", y = "Average BOGO Completion")

# Discount Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster4, y = Avg_Discount_Completion, fill = Cluster4)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Discount Offer Completion by Cluster", x = "Cluster", y = "Average Discount Completion")

melted_data <- melt(offer_response_summary, id.vars = "Cluster4")

ggplot(melted_data, aes(x = variable, y = Cluster4, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Offer Completion Rate Heatmap by Cluster", x = "Offer Type", y = "Cluster")


chisq.test(table(valid_df$Cluster4, valid_df$offer_type1)) 
# significant association between the clusters and how they respond to offer type 1

valid_df %>% group_by(Cluster4) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))

v1<-valid_df %>% group_by(Cluster4, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1)) 
v2<-valid_df %>% group_by(Cluster4, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2)) 
v3<-valid_df %>% group_by(Cluster4, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3))
v4<-valid_df %>% group_by(Cluster4, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4)) 
v5<-valid_df %>% group_by(Cluster4, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5))
v6<-valid_df %>% group_by(Cluster4, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))


total_offer_behaviour<- data.frame(v1$Cluster4, v1$offer_type1, v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5])
total_offer_behaviour
valid_df$offer_type4
bogo_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="bogo")
disc_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="discount")
none_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="none")
info_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="informational")

v1[1:4,]
total_offer_behaviour[1:4,]
t.test(bogo_clust1_period1$log_tot_trans,none_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)

t.test(bogo_clust1_period1$log_tot_amount,none_clust1_period1$log_tot_amount)
t.test(bogo_clust1_period1$log_tot_amount,disc_clust1_period1$log_tot_amount)
t.test(none_clust1_period1$log_tot_amount,disc_clust1_period1$log_tot_amount)
t.test(info_clust1_period1$log_tot_amount,disc_clust1_period1$log_tot_amount)
t.test(none_clust1_period1$log_tot_amount,info_clust1_period1$log_tot_amount)


#########################
##investigate 5 clusters
########################
valid_df %>% gather(c(age,income, tenure, tot_amount,tot_reward_rec),
                    key="Parameter",
                    value="Value")%>%
  ggplot(aes(x=Value,fill=Cluster5))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey","green", "purple"))+
  my_theme()

valid_df %>%gather(age,income, tenure, tot_amount,tot_reward_rec, 
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster5, y=Value, fill = Cluster5))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap3 )

valid_df %>%gather(age,income, tenure, tot_amount, bogo_comp, disc_comp,  
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster5, y=Value, fill = Cluster5))+
  geom_boxplot(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap3 )

valid_df %>% ggplot(aes(x=age, y=tot_amount)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster5, col = Cluster5, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap3) +
  scale_color_manual(values = clust_colmap3)

valid_df %>% ggplot(aes(x=income, y=tot_trans)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster5, col = Cluster5, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap3) +
  scale_color_manual(values = clust_colmap3)

# Scatter plot for two variables with clusters colored
ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster5)) +
  geom_point(alpha = 0.5) +
  theme_minimal()

# Box plot for comparing a variable's distribution across clusters
ggplot(valid_df, aes(x = Cluster5, y = tot_amount, fill = Cluster5)) +
  geom_boxplot() +
  theme_minimal()

# Density plot for a single variable
ggplot(valid_df, aes(x = tot_amount, fill = Cluster5)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x = ave_amount, fill = Cluster5)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x=Cluster5,y = ave_amount, fill = Cluster5)) +
  geom_boxplot() +
  ylim(c(0,50))+
  theme_minimal()

cor_res <- cor(select(valid_df, where(is.numeric)))
corrplot::corrplot(cor_res, method = "circle")

table <- valid_df %>%
  group_by(Cluster5) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

view(table)

valid_df %>%
  group_by(Cluster5,gender) %>%
  count()

ggplot(valid_df, aes(Cluster5))+
  geom_bar(aes(fill=gender,y=after_stat(count/sum(count))))


ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster5)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Age vs Total Amount Spent by Cluster", x = "Age", y = "Total Amount Spent")

ggplot(valid_df, aes(x = Cluster5, y = income, fill = Cluster5)) +
  geom_violin(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income") +
  scale_fill_manual(values = c("blue", "red", "grey","green", "purple"))

ggplot(valid_df, aes(x = tenure, y = tot_reward_rec, color = Cluster5)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tenure vs Total Rewards Received by Cluster", x = "Tenure", y = "Total Rewards Received")


# Recency Score Distribution
ggplot(valid_df, aes(x = Cluster5, y = r_score, fill = Cluster5)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Recency Score Distribution by Cluster", x = "Cluster", y = "Recency Score") +
  scale_fill_manual(values = c("blue", "red", "grey","green", "purple"))

# Frequency Score Distribution
ggplot(valid_df, aes(x = Cluster5, y = f_score, fill = Cluster5)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Frequency Score Distribution by Cluster", x = "Cluster", y = "Frequency Score") +
  scale_fill_manual(values = c("blue", "red", "grey","green", "purple"))

# Monetary Score Distribution
ggplot(valid_df, aes(x = Cluster5, y = m_score, fill = Cluster5)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monetary Score Distribution by Cluster", x = "Cluster", y = "Monetary Score") +
  scale_fill_manual(values = c("blue", "red", "grey","green", "purple"))

# Frequency vs. Monetary Score Scatter Plot
ggplot(valid_df, aes(x = f_score, y = m_score, color = Cluster5)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Frequency vs. Monetary Score by Cluster", x = "Frequency Score", y = "Monetary Score") +
  scale_color_manual(values = c("blue", "red", "grey","green", "purple"))



cor_res <- cor(select(valid_df[valid_df$Cluster5 == "1",], c(r_score, f_score, m_score)))

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
  group_by(Cluster5) %>%
  summarise(
    Avg_BOGO_Completion = mean(bogo_comp, na.rm = TRUE),
    Avg_Discount_Completion = mean(disc_comp, na.rm = TRUE),
    Avg_Total_Completion = mean(tot_off_comp, na.rm = TRUE)
  )

# BOGO Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster5, y = Avg_BOGO_Completion, fill = Cluster5)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average BOGO Offer Completion by Cluster", x = "Cluster", y = "Average BOGO Completion")

# Discount Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster5, y = Avg_Discount_Completion, fill = Cluster5)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Discount Offer Completion by Cluster", x = "Cluster", y = "Average Discount Completion")

melted_data <- melt(offer_response_summary, id.vars = "Cluster5")

ggplot(melted_data, aes(x = variable, y = Cluster5, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Offer Completion Rate Heatmap by Cluster", x = "Offer Type", y = "Cluster")


chisq.test(table(valid_df$Cluster5, valid_df$offer_type1)) 
# significant association between the clusters and how they respond to offer type 1
valid_df %>% group_by(Cluster5) %>% count()

valid_df %>% group_by(Cluster5) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))

v1<-valid_df %>% group_by(Cluster5, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1)) 
v2<-valid_df %>% group_by(Cluster5, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2)) 
v3<-valid_df %>% group_by(Cluster5, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3))
v4<-valid_df %>% group_by(Cluster5, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4)) 
v5<-valid_df %>% group_by(Cluster5, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5))
v6<-valid_df %>% group_by(Cluster5, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))


total_offer_behaviour<- data.frame(v1$Cluster5, v1$offer_type1, v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5])
total_offer_behaviour
valid_df$offer_type4
bogo_clust1_period1<-valid_df %>% filter(Cluster5==1, offer_type1=="bogo")
disc_clust1_period1<-valid_df %>% filter(Cluster5==1, offer_type1=="discount")
none_clust1_period1<-valid_df %>% filter(Cluster5==1, offer_type1=="none")
info_clust1_period1<-valid_df %>% filter(Cluster5==1, offer_type1=="informational")

v1[1:4,]
total_offer_behaviour[1:4,]
t.test(bogo_clust1_period1$log_tot_trans,none_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)

t.test(bogo_clust1_period1$log_tot_amount,none_clust1_period1$log_tot_amount)
t.test(bogo_clust1_period1$log_tot_amount,disc_clust1_period1$log_tot_amount)
t.test(none_clust1_period1$log_tot_amount,disc_clust1_period1$log_tot_amount)
t.test(info_clust1_period1$log_tot_amount,disc_clust1_period1$log_tot_amount)
t.test(none_clust1_period1$log_tot_amount,info_clust1_period1$log_tot_amount)