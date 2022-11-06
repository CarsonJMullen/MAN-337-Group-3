library(tidyverse)
library(modelr)
library(rsample)
library(mosaic)

#grab data

read.csv("Sspect2022Class.csv")

#ONLY use training_data to make model, testing_data to test model

#Data Cleaning
Sspect2022Class <- Sspect2022Class %>%
  mutate(Pts = (outcome * 2) + (outcome*1*three))

#separating data into testing/training set
data_split <- initial_split(Sspect2022Class, prop = 0.8)
training_data <- training(data_split)
testing_data <- testing(data_split)

#Model 1: Only Checking for Assists
lm_assists_only <- lm(Pts ~ assisted, data = training_data)
rmse(lm_assists_only, testing_data)

#Model 2: Predicting Points by Assists, Distance, and their Interaction
lm_assists_dist <- lm(Pts ~ assisted * distance, data = training_data)
summary(lm_assists_dist)
rmse(lm_assists_dist, testing_data)

#Model 3: Predicting Points by if Assisted, if Three, and Distance
model3 <- lm(Pts ~ (assisted + three + distance)^2, data = training_data)
summary(model3)
rmse(model3, testing_data)

#Model 4: Add Closest Defender
model4 <- lm(Pts ~ (assisted + three + distance + closestDefDist)^2, data = training_data)
summary(model4)
rmse(model4, testing_data)

#Model 5: Trying No Interactions
model5 <- lm(Pts ~ (assisted + three + distance + closestDefDist), data = training_data)
summary(model5)
rmse(model5, testing_data)

#Model 6: Trying Step Function to Speed Up Process
lm_step = step(model4, scope=~(. + shotClock + shotType + catchAndShoot + releaseTime + I(releaseTime^2)))
summary(lm_step)
rmse(lm_step, testing_data)
