# Credit Card Marketing Study Analysis
# Senior Capstone Project
# Oliver Price and Wesley Nelson
# Date: February 28, 2025

# ==============================
#     SETUP AND DATA LOADING
# ==============================

# Load required libraries
library(readxl)     # For reading Excel files
library(dplyr)      # For data manipulation
library(tidyr)      # For data cleaning
library(ggplot2)    # For data visualization
library(ROCR)       # For ROC plots and evaluation metrics
library(scales)     # For percentage formatting


# Load the dataset
credit_data <- read_excel("C:/Classwork/R Programming/Senior Capstone Semester 1/Credit Card Marketing Study Data.xlsx")

# Display the structure of the data
str(credit_data)

# Check the first few rows
head(credit_data)

# Check dimensions of the dataset
dim(credit_data)

# Rename columns to remove spaces and special characters
names(credit_data) <- gsub(" ", "_", names(credit_data))
names(credit_data) <- gsub("#", "Num", names(credit_data))
names(credit_data) <- gsub("-", "_", names(credit_data))


# Check column names after renaming
colnames(credit_data)

# Check for missing values
missing_values <- colSums(is.na(credit_data))
print(missing_values)

# Handle missing values
credit_data_clean <- na.omit(credit_data)

# Check dimensions after removing NA values
dim(credit_data_clean)

# Convert categorical variables to factors
credit_data_clean$Offer_Accepted <- factor(credit_data_clean$Offer_Accepted, 
                                           levels = c("No", "Yes"))
credit_data_clean$Reward <- factor(credit_data_clean$Reward)
credit_data_clean$Mailer_Type <- factor(credit_data_clean$Mailer_Type)
credit_data_clean$Income_Level <- factor(credit_data_clean$Income_Level, 
                                         levels = c("Low", "Medium", "High"))
credit_data_clean$Overdraft_Protection <- factor(credit_data_clean$Overdraft_Protection)
credit_data_clean$Credit_Rating <- factor(credit_data_clean$Credit_Rating, 
                                          levels = c("Low", "Medium", "High"))
credit_data_clean$Own_Your_Home <- factor(credit_data_clean$Own_Your_Home)



# ========================================
#    Exploratory Data Analysis (EDA)
# ========================================

# Basic statistics summary
summary(credit_data_clean)

# Distribution of categorical variables

# Acceptance rate
acceptance_count <- table(credit_data_clean$Offer_Accepted)
acceptance_percent <- prop.table(acceptance_count) * 100
print(acceptance_count)
print(acceptance_percent)

# Create a bar plot for acceptance rate
barplot(acceptance_count, main="Credit Card Offer Acceptance", 
        xlab="Offer Accepted", ylab="Count", col=c("lightblue", "salmon"))

# Analyze categorical variables
cat_vars <- c("Reward", "Mailer_Type", "Income_Level", "Overdraft_Protection", 
              "Credit_Rating", "Own_Your_Home")

for(var in cat_vars) {
  # Create a table of counts
  tab <- table(credit_data_clean[[var]], credit_data_clean$Offer_Accepted)
  print(var)
  print(tab)
  print(prop.table(tab, margin = 1) * 100) # Percentage by row
}


# Boxplots for numeric variables
numeric_vars <- c("Num_Bank_Accounts_Open", "Num_Credit_Cards_Held", 
                  "Num_Homes_Owned", "Household_Size", "Average_Balance",
                  "Q1_Balance", "Q2_Balance", "Q3_Balance", "Q4_Balance")

# Create boxplots for each numeric variable
par(mfrow=c(2,2)) 
for(var in numeric_vars) {
  boxplot(credit_data_clean[[var]] ~ credit_data_clean$Offer_Accepted, 
          main=paste(var, "by Offer Acceptance"),
          xlab="Offer Accepted", ylab=var, col=c("lightblue", "salmon"))
}

# Reset the plotting window
par(mfrow=c(1,1))

# Histograms for numeric variables
for(var in numeric_vars) {
  hist(credit_data_clean[[var]], main=paste("Distribution of", var),
       xlab=var, col="steelblue", border="white")
}





# ==============================================================================
#    Logistic Regression
# ==============================================================================


# Convert Offer_Accepted to binary (1/0)
# First convert to character to avoid issues with factor levels
credit_data_clean$Offer_Accepted_binary <- as.numeric(as.character(credit_data_clean$Offer_Accepted) == "Yes")

# Check conversion
table(credit_data_clean$Offer_Accepted, credit_data_clean$Offer_Accepted_binary)

# Build a simple logistic regression model only using certain variables
model <- glm(Offer_Accepted_binary ~ Credit_Rating + Income_Level + Reward + 
               Num_Credit_Cards_Held + Average_Balance + Q4_Balance + 
               Own_Your_Home + Mailer_Type,
             family = binomial, 
             data = credit_data_clean)

# Display model summary
summary(model)

# 2. Calculate predicted probabilities
credit_data_clean$predicted_prob <- predict(model, newdata = credit_data_clean, type = "response")

# Create a dataframe with actual outcomes and predicted probabilities
pred_data <- data.frame(
  actual = credit_data_clean$Offer_Accepted_binary,
  predicted = credit_data_clean$predicted_prob
)



#=============================
# 1. GAINS TABLE
#=============================

# Number of deciles
num_deciles <- 10

# Sort by predicted probability in descending order
sorted_data <- pred_data[order(-pred_data$predicted), ]

# Calculate total number of observations and observations per decile
total_obs <- nrow(sorted_data)
obs_per_decile <- ceiling(total_obs / num_deciles)

# Create gains table with equal population per bucket
gains_table <- data.frame(
  Decile = 1:num_deciles,
  Population = numeric(num_deciles),
  Targets = numeric(num_deciles),
  Non_Targets = numeric(num_deciles),
  Cum_Population = numeric(num_deciles),
  Cum_Targets = numeric(num_deciles),
  Cum_Non_Targets = numeric(num_deciles),
  Response_Rate = numeric(num_deciles),
  Cum_Response_Rate = numeric(num_deciles),
  Lift = numeric(num_deciles),
  Cum_Lift = numeric(num_deciles)
)

# Calculate the total number of targets and non-targets
total_targets <- sum(sorted_data$actual)
total_non_targets <- total_obs - total_targets
base_rate <- total_targets / total_obs

# Fill in the gains table
start_idx <- 1
for (i in 1:num_deciles) {
  end_idx <- min(start_idx + obs_per_decile - 1, total_obs)
  
  # Handle the last bucket which might have fewer observations
  if (i == num_deciles) {
    end_idx <- total_obs
  }
  
  decile_data <- sorted_data[start_idx:end_idx, ]
  
  # Count population, targets and non-targets in this decile
  gains_table$Population[i] <- nrow(decile_data)
  gains_table$Targets[i] <- sum(decile_data$actual)
  gains_table$Non_Targets[i] <- gains_table$Population[i] - gains_table$Targets[i]
  
  # Calculate cumulative metrics
  gains_table$Cum_Population[i] <- ifelse(i == 1, gains_table$Population[i], 
                                          gains_table$Population[i] + gains_table$Cum_Population[i-1])
  gains_table$Cum_Targets[i] <- ifelse(i == 1, gains_table$Targets[i], 
                                       gains_table$Targets[i] + gains_table$Cum_Targets[i-1])
  gains_table$Cum_Non_Targets[i] <- ifelse(i == 1, gains_table$Non_Targets[i], 
                                           gains_table$Non_Targets[i] + gains_table$Cum_Non_Targets[i-1])
  
  # Calculate response rates and lift
  gains_table$Response_Rate[i] <- gains_table$Targets[i] / gains_table$Population[i]
  gains_table$Cum_Response_Rate[i] <- gains_table$Cum_Targets[i] / gains_table$Cum_Population[i]
  gains_table$Lift[i] <- gains_table$Response_Rate[i] / base_rate
  gains_table$Cum_Lift[i] <- gains_table$Cum_Response_Rate[i] / base_rate
  
  # Move to the next decile
  start_idx <- end_idx + 1
}

# Add percentage of population and targets
gains_table$Pct_Population <- gains_table$Population / total_obs * 100
gains_table$Cum_Pct_Population <- gains_table$Cum_Population / total_obs * 100
gains_table$Pct_Targets <- gains_table$Targets / total_targets * 100
gains_table$Cum_Pct_Targets <- gains_table$Cum_Targets / total_targets * 100

# Display the gains table
print(gains_table)

#=============================
# 2. RANK ORDER CHART
#=============================

# Create a rank order plot
rank_order_plot <- ggplot(gains_table, aes(x = Decile)) +
  geom_line(aes(y = Response_Rate*100, color = "Response Rate"), size = 1) +
  geom_point(aes(y = Response_Rate*100, color = "Response Rate"), size = 3) +
  geom_hline(yintercept = base_rate*100, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Response Rate" = "blue")) +
  scale_x_continuous(breaks = 1:10) +  # Set breaks at every decile
  labs(
    title = "Rank Order Plot",
    subtitle = "Response Rate by Decile",
    x = "Decile",
    y = "Response Rate (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

print(rank_order_plot)

#=============================
# 3. CUMULATIVE LIFT PLOT
#=============================

# Create a lift plot
lift_plot <- ggplot(gains_table, aes(x = Decile)) +
  geom_line(aes(y = Cum_Lift, color = "Cumulative Lift"), size = 1) +
  geom_point(aes(y = Cum_Lift, color = "Cumulative Lift"), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Cumulative Lift" = "blue")) +
  scale_x_continuous(breaks = 1:10) +  # Set breaks at every decile
  labs(
    title = "Lift Chart",
    subtitle = "Cumulative Lift by Decile",
    x = "Decile",
    y = "Lift",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

print(lift_plot)

#=============================
# 4. ROC CURVE using ROCR
#=============================

# Create prediction object for ROCR
pred_obj <- prediction(pred_data$predicted, pred_data$actual)

# Calculate performance metrics for ROC
perf_roc <- performance(pred_obj, "tpr", "fpr")

# Calculate AUC
auc <- performance(pred_obj, "auc")@y.values[[1]]

# Create dataframe for ggplot
roc_df <- data.frame(
  FPR = perf_roc@x.values[[1]],
  TPR = perf_roc@y.values[[1]]
)

# Plot ROC curve
roc_plot <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curve",
    subtitle = paste("AUC =", round(auc, 3)),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  coord_equal()

print(roc_plot)



#=============================
# 5. KOLMOGOROV-SMIRNOV (KS) PLOT
#=============================

# Create performance objects for TPR and FPR
perf_tpr <- performance(pred_obj, "tpr")
perf_fpr <- performance(pred_obj, "fpr")

# Get KS statistic and cutoff
ks_stats <- perf_tpr@y.values[[1]] - perf_fpr@y.values[[1]]
max_ks <- max(ks_stats)
max_ks_idx <- which.max(ks_stats)
cutoff <- perf_tpr@x.values[[1]][max_ks_idx]

# Create data frame for ggplot
ks_df <- data.frame(
  Cutoff = perf_tpr@x.values[[1]],
  TPR = perf_tpr@y.values[[1]],
  FPR = perf_fpr@y.values[[1]],
  KS = ks_stats
)

# Create KS plot with depth of file on x-axis (from 0 to 1)
# Calculate the depth of file
ks_df$DepthOfFile <- seq(0, 1, length.out = length(ks_df$Cutoff))

# Find max KS
max_ks_depth_idx <- which.max(ks_stats)
max_ks_depth <- ks_df$DepthOfFile[max_ks_depth_idx]

# Create the plot
ks_plot <- ggplot(ks_df, aes(x = DepthOfFile)) +
  geom_line(aes(y = TPR, color = "Cumulative % of Gas Guzzlers"), size = 1) +
  geom_line(aes(y = FPR, color = "Cumulative % of Non-Gas Guzzlers"), size = 1) +
  geom_vline(xintercept = max_ks_depth, linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = max_ks_depth - 0.05, 
    y = ks_df$FPR[max_ks_depth_idx] + 0.03,
    label = paste("KS =", round(max_ks, 3)),
    hjust = 1
  ) +
  scale_color_manual(
    values = c("Cumulative % of Gas Guzzlers" = "green", "Cumulative % of Non-Gas Guzzlers" = "red")
  ) +
  labs(
    title = "Kolmogorov-Smirnov (KS) Plot",
    subtitle = paste("Maximum KS =", round(max_ks, 3), "at depth =", round(max_ks_depth, 3)),
    x = "Depth of File (Percentage)",
    y = "Cumulative Percentage",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  coord_cartesian(ylim = c(0, 1))

print(ks_plot)


