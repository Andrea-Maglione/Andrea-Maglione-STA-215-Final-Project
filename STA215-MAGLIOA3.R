## Project: STA 145, Fall 2025, Final Project 
## Who: Andrea Maglione
## Date: 2025_12_12

# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(psych)

# Load data
dataset <- read_csv("data.csv")
View(dataset)

##################################################################################
############### Table 1: Descriptive Statistics ####################   
##################################################################################

# Yards Per Play
mean(dataset$Yards_per_play)
sd(dataset$Yards_per_play)
summary(dataset$Yards_per_play)

# Total First Downs
mean(dataset$Total_First_Downs)
sd(dataset$Total_First_Downs)
summary(dataset$Total_First_Downs)

##################################################################################
#################### Figure 1: Scatter Plot ####################   
##################################################################################

# Scatterplot: Yards Per Play vs Total First Downs
plot(dataset$Yards_per_play,
     dataset$Total_First_Downs,
     xlab = "Yards Per Play",
     ylab = "Total First Downs",
     main = "Yards Per Play vs Total First Downs")

# Add mean lines
mean_x <- mean(dataset$Yards_per_play)
mean_y <- mean(dataset$Total_First_Downs)

abline(h = mean_y, col = "black")   # horizontal
abline(v = mean_x, col = "black")   # vertical

##################################################################################
############### Figure 2: Linear Regression ####################   
##################################################################################

model <- lm(Total_First_Downs ~ Yards_per_play, data = dataset)
summary(model)

# Add regression line
abline(model, col = "red")

##################################################################################
############### Statistical Test: Correlation ####################   
##################################################################################

cor.test(dataset$Yards_per_play, dataset$Total_First_Downs)

##################################################################################
############### Figure 3: Residual Plot ####################   
##################################################################################

plot(dataset$Yards_per_play,
     residuals(model),
     main = "Residual Plot",
     xlab = "Yards Per Play",
     ylab = "Residuals")

abline(h = 0, col = "red")
