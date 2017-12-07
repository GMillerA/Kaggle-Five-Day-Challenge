#Day3_RegressionChallenge
#Multiple Linear Regression
library(ggplot2)
library(boot)
library(tidyverse)
library(readr)

nyc_census <- read_csv("nyc_census_tracts.csv")
# your work goes here! :)
head(nyc_census)
colnames(nyc_census)
#Want to predict median income by professional and office. Does there seem to be a relationship?
ggplot(nyc_census, aes(x = Professional, y = Income)) + # draw a plot
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "gaussian")) # ...from the gaussian family
ggplot(nyc_census, aes(x = Office, y = Income)) + # draw a plot
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "gaussian")) # ...from the gaussian family
#There doo seem to be some linear relationships between the features and income
model <- glm(Income ~ Professional + Office, # formula
             data = nyc_census, # dataset
             family = ("gaussian")) # fit a linear model
# output plots in a 2 x 2 grid 
par(mfrow = c(2,2)) 
# diagnostic plots
plot(model)
summary(model)
#Model seems to perform OK though there are some very high outliers. 
#Std. Errors for Professional and Office are much smaller than the Estimate
#residual deviance is about an order of magnitude below Null deviance
# added-variable plots for our model
avPlots(model)
#Professional much more strongly correlated than Office, which has more of a cloud shape than
#a linear one