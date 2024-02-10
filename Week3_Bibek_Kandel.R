rm(list=ls()) # clears workspace

#Objective 1:
#In this part of exercise, I am visualizing the median values of evapotranspiration across seasons
#This will also show quartile distribution and skewness of the data. 

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
fluxdata=read.csv("project_data.csv") 
head(fluxdata)


#Plot ET data by season
uglyplot=ggplot(data=fluxdata,aes(x=Season,y=ET,color=Season))+
  geom_point(size=2, na.rm = T, show.legend = F)+
  ggtitle("Overview of flux values across seasons")+
  ylab(expression(italic("Evapotranspiration (mm/h)")))+
  xlab(expression(italic("")))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="italic"))
uglyplot

#Boxplot of seasonal ET 
uglyplot=ggplot(data=fluxdata,aes(x=Season,y=ET,color=Season))+
  geom_boxplot(show.legend = F)+
  # stat_summary computes the statistics summary 
  stat_summary(fun.y="mean",color="red", shape=13)+
  ggtitle("Distribution of median and flux values across seasons")+
  ylab(expression(italic("Evapotranspiration (mm/h)")))+
  xlab(expression(italic("")))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="italic"))
uglyplot

#Boxplots do not show the modality of the data e.g., unimodal/bimodal/multimodal
#Violin plots are useful to visualize the modality 
uglyplot=ggplot(data=fluxdata,aes(x=Season,y=ET,fill=Season))+
  geom_violin(show.legend = F)+
  geom_boxplot(show.legend = F, width = 0.05)+
  ggtitle("Distribution and modality of flux values across seasons")+
  ylab(expression(italic("Evapotranspiration (mm/h)")))+
  xlab(expression(italic("")))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="italic"))
uglyplot

#Visualize the actual data points using jitter
uglyplot=ggplot(data=fluxdata,aes(x=Season,y=ET))+
  geom_boxplot(outlier.shape = NA, show.legend = F)+
  geom_jitter(aes(shape=Season), show.legend = F)+
  ggtitle("Distribution of flux values across seasons")+
  ylab(expression(italic("Evapotranspiration (mm/h)")))+
  xlab(expression(italic("")))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,face="italic"))
uglyplot

##Reorder a selected column in the dataframe
fluxdata$Season = factor(fluxdata$Season, levels=c("Spring", "Summer", "Autumn", "Winter"))

uglyplot0=ggplot(data=fluxdata,aes(x=Season,y=ET, fill = Season))+
  geom_violin(show.legend = F)+
  geom_boxplot(show.legend = F, width = 0.05, outlier.shape = NA)+
  ggtitle("Distribution of median ET values across seasons")+
  #geom_point(aes(color=Season),size=1,shape=19,stroke=1.1, show.legend = F) +
  #shape specifies donut, and stroke changes thickness
  ylab(expression(italic("Evapotranspiration (mm/h)")))+
  xlab("")+
  scale_colour_viridis(discrete = T)+
  theme_bw()+
  theme(axis.title=element_text(size=10),
        axis.text=element_text(size=10),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 0, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))

uglyplot0

#Objective 2:
#In this part, we will try to answer the question, "How does diurnal variation/spread look like?"
#What is the peak flux hour and the within-hour variability across seasons?

#Let's format the time column to plot hourly intervals in x-axis
#format(fluxdata$Time, format="%H:%M")
#strftime(fluxdata$Time, format="%H:%M")
fluxdata$Time <- lubridate::parse_date_time(fluxdata$Time,"H:M")
fluxdata$Time <-as.POSIXct(fluxdata$Time,format="%H")
fluxdata$Time <- hour(fluxdata$Time)

#Let's plot the daily timeseries of fluxes across seasons
uglyplot1 <- ggplot(fluxdata, mapping = aes(x = Time, y = ET, color = Year))+
  geom_point(aes(Time, ET, color = factor(Year)),show.legend = T) +
  scale_color_manual(values = c("orange", "blue"))+
  #scale_x_discrete(guide = guide_axis(check.overlap = T)) Use this to remove clutters 
  facet_wrap(vars(Season)) +
  labs(title = "Diurnal timeseries of evapotranspiration across seasons",
       x = "Hours of Day",
       y = "Evapotranspiration (mm/h)") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        axis.title=element_text(size=10),
        axis.text=element_text(size=10),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))+
  theme(plot.title = element_text(size=12))+
  scale_x_continuous(breaks = c(0, 6, 12, 18))
uglyplot1

#Objective 3:
#How does this align with other meteorological variables? What potentially drives this pattern?
#To answer this question, let's visualize how other variables vary in relation...
#...to the variability in fluxes!

#Let's check the correlations among variables to shortlist potential drivers.
#This also gives an idea of multi-collinearity among independent variables.
###correlation check between all the variables
cor_check = select(fluxdata, c(10:18))
r <- cor(cor_check, use="complete.obs")
ggcorrplot(r, lab = F, lab_size = 2, method = "square", #select lab=T to see values
           colors = c("#6D9EC1", "white", "#E46726"))+
ggtitle("Correlogram visualizes the strength of relationships among variables")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))


#Select the variables that are highly correlated (>0.5) with ET flux 
#Remove independent variables that are highly correlated (>0.7) with each other
#Calculate the mean of potential drivers and store in a new dataframe
daily_subset <- fluxdata %>%
  group_by(Time, Season) %>%
  summarize(
    meanET = mean(ET),
    meanVPD = mean(VPD),
    meanT = mean(Ta),
    meanWS = mean(WS),
    meanRn = mean(Rn)) %>%
  as.data.frame()

#Visualize and compare the diurnal pattern of possible drivers 
ggp1 <- ggplot(daily_subset, aes(x=Time, y=meanET, col=Season))+
        geom_point()+
        theme(legend.position = "none", axis.title=element_text(size=8))+
        labs(x = "", y = "ET (mm/h)")
ggp2 <- ggplot(daily_subset, aes(x=Time, y=meanVPD, col=Season))+
  geom_point()+
  theme(legend.position = "none", axis.title=element_text(size=8))+
  labs(x = "", y = "VPD (Pa)")
ggp3 <- ggplot(daily_subset, aes(x=Time, y=meanT, col=Season))+
  geom_point()+
  theme(legend.position = "none", axis.title=element_text(size=8))+
  labs(x = "Hours of Day",
       y = "Temperature (C)")
ggp4 <- ggplot(daily_subset, aes(x=Time, y=meanRn, col=Season))+
  geom_point()+
  theme(legend.position = "none", axis.title=element_text(size=8))+
  labs(x = "Hours of day", y = "Net radiation (W/m2)")

#Create plots with legend
ggp5 <- ggplot(daily_subset, aes(x=Time, y=meanET, col=Season))+
  geom_point()+
  theme(legend.position = "top")

#User defined function to extract legends from ggplot
extract_legend <- function(my_gap) {
  step1 <- ggplot_gtable(ggplot_build(my_gap))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

#Apply user defined function to extract legend
shared_legend <- extract_legend(ggp5)

#Plots with shared legend
pf <- grid.arrange(arrangeGrob(ggp1,ggp2,ggp3,ggp4, ncol = 2), nrow = 2, shared_legend,
             heights = c(4,1))
#Add common title to all the plots
annotate_figure(pf, top = text_grob("Diurnal timeseries of Evapotranspiration (ET) and its predictors across seasons", 
                color = "black", face = "bold", size = 10))

#Use scatterplots to further check if the variation in Y depends on variation in X
#Strip time off from the datetime format
fluxdata$newDate <- strptime(as.character(fluxdata$DateTime), "%m/%d/%Y")
fluxdata$Day <- format(as.Date(fluxdata$newDate), "%d")

#Aggregate the data into daily mean values 
seasonal_subset <- fluxdata %>%
  group_by(Year, Season, Day) %>%
  summarize(
  meanET = mean(ET),
  meanVPD = mean(VPD),
  meanT = mean(Ta),
  meanWS = mean(WS),
  meanRn = mean(Rn)) %>%
  as.data.frame()

#Plot the data to see the relationship among dependent and independent variables
attach(seasonal_subset)
par(mfrow=c(2,2))
plot(meanRn,meanET, pch = 21,bg = "blue",col = "blue",cex = 1,lwd = 1, xlab = "Net radiation", ylab = "ET")
plot(meanVPD, meanET,pch = 21,bg = "blue",col = "blue",cex = 1,lwd = 1, xlab = "Vapor pressure deficit", ylab = "ET")
plot(meanT, meanET,pch = 21,bg = "blue",col = "blue",cex = 1,lwd = 1, xlab = "Temperature", ylab = "ET")
plot(meanWS, meanET,pch = 21,bg = "blue",col = "blue",cex = 1,lwd = 1, xlab = "Wind speed", ylab = "ET")


#Objective 4:
#Now, I'm interested to know how things changed from 2017 to 2021
uglyplot2 <- ggplot(seasonal_subset, mapping = aes(x = meanRn, y = meanET, color = Year))+
  geom_point(aes(meanRn, meanET, color = factor(Year)),show.legend = F)+
  xlab("Net radiation") + ylab("Evapotranspiration") +
  scale_color_manual(values = c("orange", "blue")) +
  theme(axis.text.x = element_text(colour = "grey20", size = 9, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 9),
        axis.title=element_text(size=9),
        axis.text=element_text(size=9),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))+
  theme(plot.title = element_text(size=12))
  #scale_x_continuous(breaks = c(0, 200, 400, 600))

uglyplot3 <- ggplot(seasonal_subset, mapping = aes(x = meanVPD, y = meanET, color = Year))+
  geom_point(aes(meanVPD, meanET, color = factor(Year)),show.legend = F)+
  xlab("Vapor pressure deficit") + ylab("Evapotranspiration") +
  scale_color_manual(values = c("orange", "blue"))+
  theme(axis.text.x = element_text(colour = "grey20", size = 9, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 9),
        axis.title=element_text(size=9),
        axis.text=element_text(size=9),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))+
  theme(plot.title = element_text(size=12))
  #scale_x_continuous(breaks = c(0, 200, 400, 600))

uglyplot4 <- ggplot(seasonal_subset, mapping = aes(x = meanT, y = meanET, color = Year))+
  geom_point(aes(meanT, meanET, color = factor(Year)),show.legend = F)+
  xlab("Temperature") + ylab("Evapotranspiration") +
  scale_color_manual(values = c("orange", "blue")) +
  theme(axis.text.x = element_text(colour = "grey20", size = 9, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 9),
        axis.title=element_text(size=9),
        axis.text=element_text(size=9),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))+
  theme(plot.title = element_text(size=12))
  #scale_x_continuous(breaks = c(0, 200, 400, 600))

uglyplot5 <- ggplot(seasonal_subset, mapping = aes(x = meanWS, y = meanET, color = Year))+
  geom_point(aes(meanWS, meanET, color = factor(Year)),show.legend = T)+
  xlab("Wind speed") + ylab("Evapotranspiration") +
  scale_color_manual(values = c("orange", "blue"))+
  theme(axis.text.x = element_text(colour = "grey20", size = 9, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 9),
        axis.title=element_text(size=9),
        axis.text=element_text(size=9),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))+
  theme(plot.title = element_text(size=12))
  #scale_x_continuous(breaks = c(0, 200, 400, 600))

#Combine all the ggplots into a single plot
uglyplot2 + uglyplot3 + uglyplot4 + uglyplot5 + plot_annotation(tag_levels = "A",
        title = "How do atmospheric drivers influence water flux across land-atmosphere interface?",
        subtitle = "How has this changed over the years?",
        caption = "Figure. Plots of evapotranspiration versus potential atmospheric drivers in 4 years' gap period",
        )














