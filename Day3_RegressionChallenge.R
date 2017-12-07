#Day3_RegressionChallenge
#Checking the fit of a regression model
library(ggplot2)
library(boot)
library(tidyverse)
library(readr)

cameras <- read_csv("camera_dataset.csv")
colnames(cameras)
# predict price given the max resolution
model <- glm(Price ~ `Max resolution`, data = cameras, family = "poisson")
#print diagnostic plots
glm.diag.plots(model)
#summary diagnostics
summary(model)
model$deviance
ggplot(cameras, aes(x = `Max resolution`, y = Price)) + # draw a plot
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "poisson")) # ...from the poisson family
#Deviance Residuals show that there are some significant problems with model
#median is -11 and quartiles are significantly apart
#another variable may perform much better

model2 <- glm(Price ~ `Low resolution`, data = cameras, family = 'poisson')
#print diagnostic plots
glm.diag.plots(model2)
summary(model2)
ggplot(cameras, aes(x = `Low resolution`, y = Price)) + # draw a plot
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "poisson")) # ...from the poisson family

#Low resolution does not perform any better than max resolution 