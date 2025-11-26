#Set working directory
setwd("courses/STA145/maglioa3")

# Load data
data <- read.csv("data.csv")

# Create boxplot: Yards per play by game outcome
ggplot(data, aes(x = Outcome, y = Yards_per_play)) +
  geom_boxplot(na.rm = TRUE, fill = "lightblue", color = "darkblue") +
  labs(title = "Yards per Play by Game Outcome",
       x = "Game Outcome",
       y = "Yards per Play")

####### Set working directory
setwd("courses/STA145/maglioa3")

####### Upload data
library(readr)
data <- read_csv("data.csv")
library(ggplot2)

####### Check structure
str(data)

####### Convert time variable (optional)
# Convert "HH:MM:SS" into total minutes
library(dplyr)
library(lubridate)

data <- data %>%
  mutate(Time_of_possession_min = period_to_seconds(hms(Time_of_possession_())) / 60)

####### Qualitative variables
# Frequency tables
table(data$opponent)
table(data$Outcome)
table(data$season)

####### Quantitative variables
summary(select(data, Game, Total_Penalties, Rushing_First_Downs, 
               fourthdown_efficency, rushing_touchdowns, Yards_per_play, 
               rushing_yards, Third_Down_Efficiency, Interceptions, 
               Passing_Touchdown, Total_First_Downs, Feild_goals_made, 
               Punt_Yards, Time_of_possession_min))

####### Optional: Export descriptive statistics table
library(psych)
desc_table <- psych::describe(select(data, Game, Total_Penalties, Rushing_First_Downs, 
                                     fourthdown_efficency, rushing_touchdowns, Yards_per_play, 
                                     rushing_yards, Third_Down_Efficiency, Interceptions, 
                                     Passing_Touchdown, Total_First_Downs, Feild_goals_made, 
                                     Punt_Yards, Time_of_possession_min))

write.csv(desc_table, "descriptive_statistics.csv", row.names = TRUE)






ggplot(data, aes(x = Outcome, y = Yards_per_play)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Yards per Play by Game Outcome",
       x = "Game Outcome",
       y = "Yards per Play")

describe(data$Total_Penalties)
describe(data$Rushing_First_Downs)
describe(data$Outcome)
describe(data$)

#descriptive stats table 
# Install needed packages (run once)
install.packages("dplyr")
install.packages("psych")

# Load packages
library(dplyr)
library(psych)

# --------------------------
# Enter your data
# --------------------------

yards_per_play <- c(
  5.48,5.88,4.85,6.14,6.55,5.17,6.08,4.61,6.25,6.38,5.75,5.74,5.35,5.17,3.54,3.98,
  5.9,4.1,3.4,6.3,6.6,5.6,4.3,6.4,5.5,5.3,3.9,4.3,4.8,6.7,5.2,6.7,5.52,4.12,6.98,
  6.07,4.98,4.43,4.1,5.99,5.64,4.22,5.03,4.49,5.04,4.05,5.2,4.61,3.63,5.49,4.92,
  5.68,6.62,5.57,5.26,6.37,6.79,4.08,5.68,6.06,5.33,6.25,7.11,5.07
)

first_downs <- c(
  18,25,21,23,16,15,19,16,22,17,17,23,20,20,10,11,23,17,18,14,34,24,20,25,29,21,
  14,15,20,19,23,23,23,21,25,18,12,15,21,21,25,20,20,22,24,27,15,24,18,24,26,25,
  21,23,21,22,23,13,19,28,16,17,25,25
)

# --------------------------
# Create a single dataframe
# --------------------------

df <- data.frame(
  Yards_Per_Play = yards_per_play,
  First_Downs = first_downs
)

# --------------------------
# Descriptive Statistics Table
# --------------------------

describe(df)
