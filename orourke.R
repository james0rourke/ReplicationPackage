## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(data$attitude_towards_drugs)
sd(data$attitude_towards_drugs)
table(data$attitude_towards_drugs)
describe(data$attitude_towards_drugs)
summary(data$attitude_towards_drugs)

mean(data$age)
sd(data$age)
table(data$age)
describe(data$age)
summary(data$age)
##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
ggplot(data = data, aes(x = race, y = age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age and Race",
       x = "Race",
       y = "Age") +
  theme_minimal()


ggplot(data = data, aes(x = occupation, y = attitude_towards_drugs)) +
  geom_boxplot() +
  labs(title = "Box Plot of Occupation and Attitude Towards Drugs",
       x = "Occupation",
       y = "Attitude Towards Drugs") +
  theme_minimal()


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
ggplot(data, aes(x = age, y = attitude_towards_drugs)) +
  geom_point() +
  labs(title = "Scatter Plot of Age and Attitude Towards Drugs",
       x = "Age",
       y = "Attitude Towards Drugs") +
  theme_minimal()

lm_model <- lm(attitude_towards_drugs ~ age, data = data)

summary(lm_model)

ggplot(data, aes(x = age, y = attitude_towards_drugs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Age and Attitude Towards Drugs with Regression Line",
       x = "Age",
       y = "Attitude Towards Drugs") +
  theme_minimal()


##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
lm_model <- lm(attitude_towards_drugs ~ age, data = data)

residuals <- resid(lm_model)

residuals_df <- data.frame(age = data$age, residuals = residuals)

mean_residual <- mean(residuals)

scatter <- ggplot(residuals_df, aes(x = age, y = residuals)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Residuals",
       x = "Age",
       y = "Residuals") +
  theme_minimal()

scatter_with_line <- scatter + 
  geom_hline(yintercept = mean_residual, color = "red", linetype = "dashed") +
  annotate("text", x = max(residuals_df$age), y = mean_residual, 
           label = paste("Mean Residual:", round(mean_residual, 2)), 
           hjust = 1.1, vjust = 0.5, color = "red")

print(scatter_with_line)


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$attitude_towards_drugs, data$occupation)
chisq.test(data$attitude_towards_drugs, data$occupation)
