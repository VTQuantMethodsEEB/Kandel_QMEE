# Kandel_QMEE
This repo contains all the information required for Quantitative methods in Ecology and Evolution.
<p> This is a modified code from R Studio.
<p> Make sure you save, commit and push all the edits to your remote repo.
<p> Thanks!

##Clean and organize data for the project
library(tidyverse)
df <- read.csv("Hourly_SEM.csv")
project_data <- subset(df, Year == "2017" | Year == "2021")
write.csv(project_data, "project_data.csv", row.names = T)
#The end