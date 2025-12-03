## Project: STA 145, Fall 2025, Final Project 
## Who: Andrea Maglione

##Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(psych)

# Load data
dataset <- read_csv("data.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(dataset$Yards_per_play)
sd(dataset$Yards_per_play)
summary(data$Yards_per_play)

mean(dataset$Total_First_Downs)
sd(dataset$Total_First_Downs)
summary(dataset$Total_First_Downs)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################

# Scatterplot: Yards Per Play vs Total First Downs
linear_plot <- plot(dataset$Yards_per_play,
                    dataset$Total_First_Downs,
                    xlab = "Yards Per Play",
                    ylab = "Total First Downs",
                    main = "Yards Per Play vs Total First Downs")

# Add mean lines
meany <- mean(dataset$Yards_per_play)
meanx <- mean(dataset$Total_First_Downs)

abline(h = meanx, col = "black")   # horizontal mean line
abline(v = meany, col = "black")   # vertical mean line

##################################################################################
############### Test 1: Linear regression  ####################   
##################################################################################
linear_relationship <- lm(Total_First_Downs ~ Yards_per_play, data = dataset)
summary(linear_relationship)

# Add regression line
abline(linear_relationship, col = "red")


##################################################################################
############### Test 2:Correlation Test ####################   
##################################################################################
cor.test(dataset$Yards_per_play, dataset$Total_First_Downs)

#Linear Regression 
model <- lm(Total_First_Downs ~ Yards_per_play, data = dataset)
summary(model)

