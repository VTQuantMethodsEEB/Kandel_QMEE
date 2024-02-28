rm(list=ls()) # clears workspace

#Objective 1:
#In this part of exercise, I am formulating some hypotheses about my data and testing them.
#H1: The evapotranspiration data for both the years are normally distributed.
#H2: The mean evapotranspiration values for summer is significantly higher than that for spring season.
#H3: The mean evapotranspiration values for the year 2021 is significantly higher than that for 2017....
#...owing to the increase in temperature and vapor pressure deficit following global warming.

#load important packages##
library(ggplot2)
library(gridExtra)
library(viridis)
library(tidyverse)
library(scales)
library(forcats)
library(lubridate)
library(ggcorrplot)
library(patchwork)
library(ggpubr)

#Read the dataset
fluxdata <- read.csv("project_data.csv")
ET_data <- fluxdata %>%
  select(c(Date, Year, Season, ET)) %>% filter(Season == "Spring" | Season == "Summer")
head(ET_data)

#H1: The evapotranspiration data for both the seasons are normally distributed.
#Testing the hypothesis using Shapiro-Wilk test of normality
#Are our data normally distributed?##
#The null hypothesis is that the data are normally distributed
#P<0.05 indicates NOT normal
Sp_data <- ET_data %>% filter(Season == "Spring")
swt_spring<-shapiro.test(Sp_data$ET)
swt_spring

Su_data <- ET_data %>% filter(Season == "Summer")
swt_summer<-shapiro.test(Su_data$ET)
swt_summer

#Result: The seasonal evapotranspiration data for spring and summer season are NOT normally distributed.
#Hence, I was not able to reject null hypothesis.

#H2: The mean evapotranspiration values for summer and winter season are significantly different.
#Testing the hypothesis using permutation test
#The null hypothesis is that there's no difference in mean spring and summer evapotranspiration values 
#Compute the seasonal mean values from the subset of fluxdata for ET for spring and summer.
#Calculate the mean
m_Sp = mean(Sp_data$ET)

m_Su = mean(Su_data$ET)

#the difference in the mean 
diff_mean = m_Su - m_Sp

##how to write your own permutation test##
set.seed(101)
#set.seed will set R's random number generator to start at the same place
#this ensures that when you, and I, and anyone else, does the test, we will all get the same results

res <- NA ## set aside space for results

#you always need to do something like this when you run a "for" loop
#you could also write res <- numeric(1000), which would give you a list of 1000 0's
#the important thing to have a vector already named "res"
#Store table values as list of numbers by unlisting them

Spring_values <- as.numeric(unlist(Sp_data$ET))
Summer_values <- as.numeric(unlist(Su_data$ET))
sample(Spring_values, 4031, replace=F) #spring has 4031 values

#use jar example to illustrate sampling procedure 

for (i in 1:10000) {
  ET_boot <- sample(c(Spring_values, Summer_values)) ## scramble
  ## pick out forest & field samples
  Spring_boot <- ET_boot[1:length(Spring_values)] #this says assign the first six colonies to forest
  Summer_boot <- ET_boot[(length(Summer_values)+1):length(ET_boot)] #assign the rest of the colonies to field
  
  #if you had a dataframe it would look like
  #forestboot <- colonyboot[1:length(ants$place[ants$place=="forest"])] #this says assign the first six colonies to forest
  #fieldboot <- colonyboot[(length(ants$place[ants$place=="forest"])+1):length(ants$place)] #this says assign the rest of the observations to field
  
  ## compute & store difference in means
  res[i] <- mean(Summer_boot) - mean(Spring_boot) #calculate the difference in the field means and the forest means
  #[i] says "where i", and i is a counter, after running this loop, i should be 1000
}

#what is our observed mean difference?
diff_mean

#Plot the results
hist(res,col="gray",las=1,main="")
abline(v=diff_mean,col="red")

##so how do we get our p-value?
res[res>=diff_mean]
l = length(res[res>=diff_mean])
l/10000
mean(res>=diff_mean)  

#Also, since the data are non-normal, I am going to use Mann-Whitney/Wilkoxon ranked sum test
#To test the difference in seasonal mean values
#More powerful for unequal sample sizes
##unpaired/If the means of the ranks in the two groups are very different, the P value will be small.

ww_s <-wilcox.test(Sp_data$ET,Su_data$ET)
ww_s

#Result: Mean ET for summer (0.17 mm/h) was significantly (p<0.01) higher than that for spring (0.09 mm/h).
#############


#H3: The mean evapotranspiration value for the year 2021 is significantly higher than that for 2017.
#Test of normality
data2017 <- ET_data %>% filter(Year == "2017")
swt_2017<-shapiro.test(data2017$ET)
swt_2017

data2021 <- ET_data %>% filter(Year == "2021")
swt_2021<-shapiro.test(data2021$ET)
swt_2021

#Calculate the annual mean
m2017 <- mean(data2017$ET)
m2021 <- mean(data2021$ET)
mean_diff <- m2017 - m2021

##unpaired/If the means of the ranks in the two groups are very different, the P value will be small.

ww_y<-wilcox.test(data2017$ET,data2021$ET)
ww_y

#Result: Mean ET for 2017 (0.139 mm/h) was significantly (p<0.01) higher than that for 2021 (0.122 mm/h).
#Hence, I was able to reject null hypothesis that annual hourly mean is not different for these two years.
#However, the decrease in annual hourly ET despite regional and global rise in temperature and vapor pressure deficit...
#...is very surprising. As ET is thought to be generally promoted by higher temperature and VPD values ...
#...given water availability, it'd be interesting to see how this ecosystem responds to long-term change in climate.
#############

