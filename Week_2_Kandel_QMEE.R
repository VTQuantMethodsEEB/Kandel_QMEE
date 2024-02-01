#Initialize libraries
library(tidyverse)
library(dplyr)

#Read your data into R.
df <- read.csv("project_data.csv")

#Examine the dataset for mistakes.
#Check first six rows
head(df)

#Check last six rows
tail(df)

#Check the variable names and character types
str(df)

#Plot your data to examine quick trends
plot(df$Ta, df$ET, pch=19)
hist(df$ET)
boxplot(df$ET~df$Season, pch=19)

#Experiment with 'group_by' in dplyr to do some calculations. 
#Calculate the mean ET and standard deviation for different seasons
df %>%
  group_by(Season) %>%
  summarize(
    m = mean(ET, na.rm=TRUE),
    sd = sd(ET),
  ) 

#Use summarize and mutate to assess the difference.
df %>% 
  group_by(Season)
mutate(
  df, ET_from_LE = LE*0.035)

df %>%
  summarise( 
    m = median(ET),
    data_range = range(ET),
    first_quartile = quantile(ET, 0.25),
    third_quartile = quantile(ET, 0.75),
  )

#mutate creates a new column and returns the same number of rows in a data frame
#summarize returns just one row.
#The end!

