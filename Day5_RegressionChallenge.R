#Day5 Regression Challenge
#Elastic Net Feature Selection
# libraries
library(tidyverse)
library(glmnet) # fit glms with elastic net
library(car) # for avplots

# read in data
coders_2017 <- read_csv("2017-fCC-New-Coders-Survey-Data.csv")

dataSubset17 <- coders_2017 %>%
  mutate(IsWoman = (Gender == "female")) %>% # convert gender to a boolean variables
  select(Age, JobInterestDataSci, HasChildren, 
         HasStudentDebt, IsWoman, HasDebt,
         HoursLearning, MonthsProgramming, Income) %>%
  na.omit() # remove non-numeric values

# convert our input variables to a matrix
input <- dataSubset17 %>%
  select(-Income) %>% # dont include the variable we're predicting!
  as.matrix()

# get a vector with our output variable
output <- dataSubset17$Income

# how many examples do we have?
print("Mean of our predicted value:")
mean(output)

# use 10-fold cross-validation to fit a bunch of models using elastic net
cv_fit <- cv.glmnet(input, output, family = "gaussian")

# get coefficents for the best model
coef(cv_fit, s = "lambda.min")

# get a (non-sparse) matrix of the coefficents for the best model
coef_matrix <- coef(cv_fit, s = "lambda.min") %>% 
  as.matrix()

# get the variables with a coefficent that's not 0 
variables <- row.names(coef_matrix)[coef_matrix != 0] %>% # get variables w/ non-zero intercepts
  setdiff("(Intercept)") #remove the intercept (if it's included)

# this variable has just our selected features. 
# print the first few
head(variables)

# turn our list of formulas into a variable
variables_selected <- paste(variables, collapse="+")
formula <- paste("Income ~ ",variables_selected,sep = "") %>%
  as.formula()

# fit a glm model
model <- glm(formula, # formula
             data = dataSubset17, # dataset
             family = ("gaussian")) # fit a poisson model

# output plots in a 2 x 2 grid 
par(mfrow = c(2,2)) 

# diagnostic plots
plot(model)

# take a closer look at our model
summary(model)

# added-variable plots for our model
avPlots(model)