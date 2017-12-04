library(ggplot2)
library(tidyverse)
library(readr)

bikes <- read_csv(".../nyc-east-river-bicycle-counts.csv")
head(bikes)
print("Is Brooklyn Bridge Integer?")
is.integer(bikes$`Brooklyn Bridge`)
print("Is Brooklyn Bridge Numeric?")
is.numeric(bikes$`Brooklyn Bridge`)
bikes$`Brooklyn Bridge` <- as.integer(bikes$`Brooklyn Bridge`)
is.integer(bikes$`Brooklyn Bridge`)
hist(bikes$`Brooklyn Bridge`)

#check for NAs
sum(is.na(bikes$`Brooklyn Bridge`))
#No NAs

#Now for the independent variable, precipitation
is.numeric(bikes$Precipitation) #not numeric
typeof(bikes$Precipitation)
bikes$Precipitation <- as.numeric(bikes$Precipitation)
sum(is.na(bikes$Precipitation)) #14 NAs introduced

ggplot(bikes, aes(x = Precipitation, y = `Brooklyn Bridge`)) + # draw a 
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
          method.args = list(family = "poisson")) # ...from the poisson family

print("Is Low Temp numeric?")
is.numeric(bikes$`Low Temp (°F)`)
#Check for NAs
sum(is.na(bikes$`Low Temp (°F)`))
#No NAs
ggplot(bikes, aes(x = `Low Temp (°F)`, y = `Brooklyn Bridge`)) + # draw a 
  geom_point()  + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "poisson")) # ...from the poisson family
