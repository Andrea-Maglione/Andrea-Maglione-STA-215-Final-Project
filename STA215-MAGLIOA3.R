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
############### Table 1: Descriptive Statistics    ####################   
##################################################################################
mean(dataset$Yards_per_play)
sd(dataset$Yards_per_play)
summary(data$Yards_per_play)

mean(dataset$Total_First_Downs)
sd(dataset$Total_First_Downs)
summary(dataset$Total_First_Downs)

##################################################################################
####################   Figure 1: Scatter Plot             ####################   
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
############### Figure 2: Linear regression  ####################   
##################################################################################
linear_relationship <- lm(Total_First_Downs ~ Yards_per_play, data = dataset)
summary(linear_relationship)

# Add regression line
abline(linear_relationship, col = "red")


##################################################################################
############### Figure 3:Correlation Test ####################   
##################################################################################
cor.test(dataset$Yards_per_play, dataset$Total_First_Downs)

#Linear Regression 
model <- lm(Total_First_Downs ~ Yards_per_play, data = dataset)
summary(model)

##################################################################################
####################  Figure 4: Residual Plot                ####################   
##################################################################################

# Run the linear regression model
linear_relationship <- lm(Total_First_Downs ~ Yards_per_play, data = dataset)

# Plot the residuals against Yards per Play
plot(dataset$Yards_per_play,
     residuals(linear_relationship),
     main = "Residual Plot",
     xlab = "Yards per Play",
     ylab = "Residuals")

# Add horizontal line at zero
abline(h = 0, col = "red")


