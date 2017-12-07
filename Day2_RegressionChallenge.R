#Day2_RegressionChallenge
#Diagnostic Plots for GLM Residuals

library(tidyverse)
library(boots)

stackOverflow <- read_csv("../survey_results_public.csv")
print('Dimensions of DF are: ')
dim(stackOverflow) #51392 x 154
print('Head of DF: ')
head(stackOverflow)
print('Colnames: ')
colnames(stackOverflow)
#Looking at salary and Hours per Week
print('Is Salary Numeric? ')
is.numeric(stackOverflow$Salary)
print('Is HoursPerWeek numeric?')
is.numeric(stackOverflow$HoursPerWeek)
print('Num of NAs for Salary: ')
sum(is.na(stackOverflow$Salary)) #38501
print('Num of NAs for HoursPerWeek')
sum(is.na(stackOverflow$HoursPerWeek)) #30792
# do some data cleaning, just want Salary and HoursPerWeek
df <- stackOverflow %>%
  filter(!is.na(Salary)) %>% # remove NAs
  filter(!is.na(HoursPerWeek)) #remove NAs
head(df)
sum(is.na(df$Salary))
sum(is.na(df$HoursPerWeek))
is.numeric(df$Salary)
is.numeric(df$HoursPerWeek)
ggplot(df, aes(x = Salary, y = HoursPerWeek)) + # draw a 
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "gaussian"))
#print('There does seem to a be negative linear relationship between Salary and HoursPerWeek')
#print('Now to check model')
model <- glm(Salary ~ HoursPerWeek, data = df, family = gaussian)
#diagnostic plots
glm.diag.plots(model)

#Looks like there are some important outliers above 40 hours per week
df <- df %>%
  filter(HoursPerWeek < 40)
model <- glm(Salary ~ HoursPerWeek, data = df, family = gaussian)
#diagnostic plots
glm.diag.plots(model)
#Performs a little better but not by much