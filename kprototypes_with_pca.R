################################################################################
## K-prototype for wide_data5 with pca variables
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

data_wide5<-read.csv("data_wide5.1.csv")
pca_comp<- read_csv("pca_components.csv")
data_wide5<-cbind(data_wide5,pca_comp)

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
colnames(data_wide5)
selected_vars <- c("gender", "PC1", "PC2","PC3","PC4","PC5","PC6" )

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

# no missing values
bar_missing(data_selected)


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
imputed_data %>% gather(c(PC1,PC2,PC3,PC4,PC5,PC6),
                        key="Parameter",
                        value="Value")%>%
  ggplot(aes(x=Value,fill=gender))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey"))+
  my_theme()

imputed_data %>%
  pivot_longer(cols = c(PC1,PC2,PC3,PC4,PC5,PC6),
               names_to = "Parameter",
               values_to = "Value") %>%
  ggplot(aes(x=Value, fill=gender)) +
  geom_density(alpha=0.5) +
  ggtitle("Hypertension") +
  facet_wrap(~Parameter, ncol=2, scales="free") +
  scale_fill_manual(values=c("blue", "red", "grey")) +
  theme_minimal() 


ggplot(imputed_data, aes(x=gender, y=PC1, fill=gender)) +
  geom_boxplot() +
  scale_fill_manual(values=c("blue", "red", "grey")) +
  theme_minimal() 


Es<-numeric(10)


for(i in 1:10){
  kpres <- kproto(data_selected, 
                  k = i, nstart = 5, 
                  lambda = lambdaest(data_selected),
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




## find k (option 2) -----------------------------------------------------------
set.seed(7)

total_withinss <- c()

for (i in 1:8) {
  kproto <- clustMixType::kproto(data_selected,
                                 k = i,
                                 nstart = 15)
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

k_opt <- validation_kproto(data = data_selected, k = 2:6, nstart = 5)
k_opt$k_opt
## CLUSTERING ##################################################################
kpres_2 = kproto(x = data_selected,
                 k = 2,
                 lambda = lambdaest(data_selected), nstart=10)


kpres_3 = kproto(x = data_selected,
                 k = 3,
                 lambda = lambdaest(data_selected), nstart=10)

kpres_4 = kproto(x = data_selected,
                 k = 4,
                 lambda = lambdaest(data_selected), nstart=10)

kpres_5 = kproto(x = data_selected,
                 k = 5,
                 lambda = lambdaest(data_selected), nstart=10)

kpres_6 = kproto(x = data_selected,
                 k = 6,
                 lambda = lambdaest(data_selected), nstart=10)
## Investigate the cluster (prototype) #########################################
valid_df = data_wide5 %>% mutate(Cluster2 = as.factor( kpres_2$cluster))
valid_df = valid_df %>% mutate(Cluster2 = as.factor( kpres_3$cluster))
valid_df = valid_df %>% mutate(Cluster4 = as.factor( kpres_4$cluster))
valid_df = valid_df %>% mutate(Cluster5 = as.factor( kpres_5$cluster))
valid_df = valid_df %>% mutate(Cluster6 = as.factor( kpres_6$cluster))
valid_df


##################
## 3 Clusters
###############

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

cor_res <- cor(select(valid_df, where(is.numeric)))
corrplot::corrplot(cor_res, method = "circle")

table <- valid_df %>%
  group_by(Cluster3) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

valid_df %>%
  group_by(Cluster3,gender) %>%
  count()

ggplot(valid_df, aes(Cluster3))+
  geom_bar(aes(fill=gender,y=after_stat(count/sum(count))))

view(table)

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

valid_df %>% group_by(Cluster3, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1)) %>% print(n=Inf)
valid_df %>% group_by(Cluster3, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2)) %>% print(n=Inf)
valid_df %>% group_by(Cluster3, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3)) %>% print(n=Inf)
valid_df %>% group_by(Cluster3, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4)) %>% print(n=Inf)
valid_df %>% group_by(Cluster3, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5)) %>% print(n=Inf)
valid_df %>% group_by(Cluster3, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))
v3
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
t.test(none_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)
t.test(disc_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)

##################
## 2 Clusters
###############

valid_df %>% gather(c(age,income, tenure, tot_amount,tot_reward_rec),
                    key="Parameter",
                    value="Value")%>%
  ggplot(aes(x=Value,fill=Cluster2))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey"))+
  my_theme()

valid_df %>%gather(age,income, tenure, tot_amount,tot_reward_rec, 
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster2, y=Value, fill = Cluster2))+
  geom_violin(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap )

valid_df %>%gather(age,income, tenure, tot_amount, bogo_comp, disc_comp,  
                   key = "Para",value="Value")%>%
  ggplot(aes(x = Cluster2, y=Value, fill = Cluster2))+
  geom_boxplot(alpha=0.5,col="black")+
  facet_wrap(~Para,ncol=2,scales = "free")+
  coord_flip()+
  scale_fill_manual(values=clust_colmap )

valid_df %>% ggplot(aes(x=age, y=tot_amount)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster2, col = Cluster2, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap) +
  scale_color_manual(values = clust_colmap)

valid_df %>% ggplot(aes(x=income, y=tot_trans)) +
  stat_density2d(
    geom = "polygon",
    aes(fill = Cluster2, col = Cluster2, alpha = after_stat(level))
  ) +
  geom_jitter(color = "black", size = 0.2, alpha = 0.3) +
  scale_fill_manual(values = clust_colmap) +
  scale_color_manual(values = clust_colmap)

# Scatter plot for two variables with clusters colored
ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster2)) +
  geom_point(alpha = 0.5) +
  theme_minimal()

# Box plot for comparing a variable's distribution across clusters
ggplot(valid_df, aes(x = Cluster2, y = tot_amount, fill = Cluster2)) +
  geom_boxplot() +
  theme_minimal()

# Density plot for a single variable
ggplot(valid_df, aes(x = tot_amount, fill = Cluster2)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

ggplot(valid_df, aes(x = ave_amount, fill = Cluster2)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

cor_res <- cor(select(valid_df, where(is.numeric)))
corrplot::corrplot(cor_res, method = "circle")

table <- valid_df %>%
  group_by(Cluster2) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

valid_df %>%
  group_by(Cluster2,gender) %>%
  count()

ggplot(valid_df, aes(Cluster2))+
  geom_bar(aes(fill=gender,y=after_stat(count/sum(count))))

view(table)

ggplot(valid_df, aes(x = age, y = tot_amount, color = Cluster2)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Age vs Total Amount Spent by Cluster", x = "Age", y = "Total Amount Spent")

ggplot(valid_df, aes(x = Cluster2, y = income, fill = Cluster2)) +
  geom_violin(alpha = 0.5, color = "black") +
  theme_minimal() +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income") +
  scale_fill_manual(values = c("blue", "red", "grey"))

ggplot(valid_df, aes(x = tenure, y = tot_reward_rec, color = Cluster2)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tenure vs Total Rewards Received by Cluster", x = "Tenure", y = "Total Rewards Received")


# Recency Score Distribution
ggplot(valid_df, aes(x = Cluster2, y = r_score, fill = Cluster2)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Recency Score Distribution by Cluster", x = "Cluster", y = "Recency Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Frequency Score Distribution
ggplot(valid_df, aes(x = Cluster2, y = f_score, fill = Cluster2)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Frequency Score Distribution by Cluster", x = "Cluster", y = "Frequency Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Monetary Score Distribution
ggplot(valid_df, aes(x = Cluster2, y = m_score, fill = Cluster2)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monetary Score Distribution by Cluster", x = "Cluster", y = "Monetary Score") +
  scale_fill_manual(values = c("blue", "red", "grey"))

# Frequency vs. Monetary Score Scatter Plot
ggplot(valid_df, aes(x = f_score, y = m_score, color = Cluster2)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Frequency vs. Monetary Score by Cluster", x = "Frequency Score", y = "Monetary Score") +
  scale_color_manual(values = c("blue", "red", "grey"))



cor_res <- cor(select(valid_df[valid_df$Cluster2 == "1",], c(r_score, f_score, m_score)))

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
  group_by(Cluster2) %>%
  summarise(
    Avg_BOGO_Completion = mean(bogo_comp, na.rm = TRUE),
    Avg_Discount_Completion = mean(disc_comp, na.rm = TRUE),
    Avg_Total_Completion = mean(tot_off_comp, na.rm = TRUE)
  )

# BOGO Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster2, y = Avg_BOGO_Completion, fill = Cluster2)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average BOGO Offer Completion by Cluster", x = "Cluster", y = "Average BOGO Completion")

# Discount Offer Completion Rate by Cluster
ggplot(offer_response_summary, aes(x = Cluster2, y = Avg_Discount_Completion, fill = Cluster2)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Discount Offer Completion by Cluster", x = "Cluster", y = "Average Discount Completion")

melted_data <- melt(offer_response_summary, id.vars = "Cluster2")

ggplot(melted_data, aes(x = variable, y = Cluster2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Offer Completion Rate Heatmap by Cluster", x = "Offer Type", y = "Cluster")


chisq.test(table(valid_df$Cluster2, valid_df$offer_type1)) 
# significant association between the clusters and how they respond to offer type 1

valid_df %>% group_by(Cluster2) %>% 
  summarise(bogo_comp=mean(bogo_response_rate, na.rm = TRUE), bogo_view=mean(bogo_view_rate, na.rm=TRUE), disc_comp=mean(disc_response_rate, na.rm=TRUE),disc_view=mean(disc_view_rate, na.rm=TRUE))

v1<-valid_df %>% group_by(Cluster2, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1)) %>% print(n=Inf)
v2<-valid_df %>% group_by(Cluster2, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2)) %>% print(n=Inf)
v3<-valid_df %>% group_by(Cluster2, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3)) %>% print(n=Inf)
v4<-valid_df %>% group_by(Cluster2, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4)) %>% print(n=Inf)
v5<-valid_df %>% group_by(Cluster2, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5)) %>% print(n=Inf)
v6<-valid_df %>% group_by(Cluster2, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))

v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5]
total_offer_behaviour<- data.frame(v1$Cluster2, v1$offer_type1, v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5])
total_offer_behaviour
valid_df$offer_type4
bogo_clust1_period1<-valid_df %>% filter(Cluster2==1, offer_type1=="bogo")
disc_clust1_period1<-valid_df %>% filter(Cluster2==1, offer_type1=="discount")
none_clust1_period1<-valid_df %>% filter(Cluster2==1, offer_type1=="none")
info_clust1_period1<-valid_df %>% filter(Cluster2==1, offer_type1=="informational")

t.test(bogo_clust1_period1$log_tot_trans,none_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)
t.test(disc_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)

##################
## 4 Clusters
###############
clust_colmap2 = c("#f7286d","#1faae0","#ffbf1f", "#33ff33")
valid_df %>% gather(c(age,income, tenure, tot_amount,tot_reward_rec),
                    key="Parameter",
                    value="Value")%>%
  ggplot(aes(x=Value,fill=Cluster4))+
  geom_density(alpha=0.5)+
  ggtitle("Hypertension")+
  facet_wrap(~Parameter,ncol=2,scales="free")+
  scale_fill_manual(values = c("blue","red","grey","yellow"))+
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
  scale_fill_manual(values = c("blue", "red", "grey","yellow"))

ggplot(valid_df, aes(x = tenure, y = tot_reward_rec, color = Cluster4)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Tenure vs Total Rewards Received by Cluster", x = "Tenure", y = "Total Rewards Received")


# Recency Score Distribution
ggplot(valid_df, aes(x = Cluster4, y = r_score, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Recency Score Distribution by Cluster", x = "Cluster", y = "Recency Score") +
  scale_fill_manual(values = c("blue", "red", "grey","yellow"))

# Frequency Score Distribution
ggplot(valid_df, aes(x = Cluster4, y = f_score, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Frequency Score Distribution by Cluster", x = "Cluster", y = "Frequency Score") +
  scale_fill_manual(values = c("blue", "red", "grey","yellow"))

# Monetary Score Distribution
ggplot(valid_df, aes(x = Cluster4, y = m_score, fill = Cluster4)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Monetary Score Distribution by Cluster", x = "Cluster", y = "Monetary Score") +
  scale_fill_manual(values = c("blue", "red", "grey","yellow"))

# Frequency vs. Monetary Score Scatter Plot
ggplot(valid_df, aes(x = f_score, y = m_score, color = Cluster4)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Frequency vs. Monetary Score by Cluster", x = "Frequency Score", y = "Monetary Score") +
  scale_color_manual(values = c("blue", "red", "grey","yellow"))



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

v1<-valid_df %>% group_by(Cluster4, offer_type1) %>% summarise(mean(num_trans1), mean(tot_amount1), mean(ave_amount1)) %>% print(n=Inf)
v2<-valid_df %>% group_by(Cluster4, offer_type2) %>% summarise(mean(num_trans2), mean(tot_amount2), mean(ave_amount2)) %>% print(n=Inf)
v3<-valid_df %>% group_by(Cluster4, offer_type3) %>% summarise(mean(num_trans3), mean(tot_amount3), mean(ave_amount3)) %>% print(n=Inf)
v4<-valid_df %>% group_by(Cluster4, offer_type4) %>% summarise(mean(num_trans4), mean(tot_amount4), mean(ave_amount4)) %>% print(n=Inf)
v5<-valid_df %>% group_by(Cluster4, offer_type5) %>% summarise(mean(num_trans5), mean(tot_amount5), mean(ave_amount5)) %>% print(n=Inf)
v6<-valid_df %>% group_by(Cluster4, offer_type6) %>% summarise(mean(num_trans6), mean(tot_amount6), mean(ave_amount6))
v3
v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5]
total_offer_behaviour<-cbind(v1[,1:2], data.frame(v1$Cluster4, v1$offer_type1, v1[,3:5]+v2[,3:5]+v3[,3:5]+v4[,3:5]+v5[,3:5]+v6[,3:5]))

colnames(total_offer_behaviour)[colnames(total_offer_behaviour) == 'offer_type1'] <- 'offer_type'
total_offer_behaviour


bogo_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="bogo")
disc_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="discount")
none_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="none")
info_clust1_period1<-valid_df %>% filter(Cluster4==1, offer_type1=="informational")

t.test(bogo_clust1_period1$log_tot_trans,none_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,disc_clust1_period1$log_tot_trans)
t.test(none_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)
t.test(bogo_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)
t.test(disc_clust1_period1$log_tot_trans,info_clust1_period1$log_tot_trans)

