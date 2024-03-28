rm(list=ls())

##linear models lecture##
##week 7###
library(performance)
library(lme4)
library(see)
library(ggplot2)

#read data
df_model <- read.csv("project_data.csv")

#Uni-variate linear model using net radiation as a predictor of fluxes
mod_uni <- lm(ET ~ Rn, data = df_model)
summary(mod_uni)

#The output showed that the rate of fluxes (ET) is positively influenced by the increasing amount of 
#net radiation (Rn) and the effect is statistically clear. This model also explained about 51% of the
#variation in fluxes.

#check the residual diagnostic plots
par(mar=c(1,1,1,1)) #Adjust the plotting panel into a desirable grid
plot(mod_uni)  #Plot the model information

#######
#Performance package and diagnostic plots
#More info and example https://easystats.github.io/see/articles/performance.html
result <- check_outliers(mod_uni)
plot(result, type = "dots")

#bars indicating influential observations
p1 <- plot(result, type = "bars")
p1 + scale_x_discrete(breaks = seq(1000,15000,1000))


#check for normally distributed residuals using a density plot
result2 <- check_normality(mod_uni)
plot(result2, type = "density")

#check qq plot
plot(result2, type = "qq")

#check posterior predictions
check_posterior_predictions(mod_uni, check_range = TRUE)

#check for homogeneity of variance
result <- check_heteroscedasticity(mod_uni)
plot(result)

mod_varcheck <- lm(ET ~ Season, data = df_model) 
result <- check_homogeneity(mod_varcheck)
suppressWarnings(plot(result))

#alternatively
bartlett.test(df_model$ET, df_model$Season)
#####

#All of these tests and plots above show that linear fit is fairly good at explaining the 
#relationship between net radiation and fluxes. The variance changes with the increase in magnitude
#of predictor variable and residuals are not normally distributed. Hence, a different modeling
#structure is needed.


#Plot using stat_smooth
c <- ggplot(df_model, aes(Rn, ET))
c + stat_smooth()
c + geom_point(alpha = 0.2) + stat_smooth(fill = "grey50", size = 0.9, color = "maroon") 


#Part 2:
#Here, I am developing a uni-variate linear model using only net radiation and then other variables 
#to test the hypothesis: The rate of fluxes is driven primarily by net radiation (Rn), 
#temperature (Ta) and Vapor Pressure Deficit (VPD).

#Uni-variate additive model using net radiation, temperature and VPD as predictors of fluxes
mod_add <- lm(ET ~ Rn + Ta + VPD, data = df_model)
summary(mod_add)

#The results showed that the effect of net radiation, temperature, and vapor pressure deficit
#on fluxes is positive and statistically clear. Among these, the effect of temperature in 
#influencing fluxes was maximum. This model explained about 55% of the variation in fluxes.

#As seen above, temperature influenced the rate of fluxes the most. I'd like to test the
#hypothesis that the effect of temperature on fluxes varies across seasons. To test that, I'm 
#using season as one of the predictors in this interactive model.

#Uni-variate interactive model using temperature and seasons as predictors of fluxes
mod_int <- lm(ET ~ Ta*Season, data = df_model)
summary(mod_int)

#The results showed that the effect of temperature on fluxes is seasonal and statistically clear.
#Among these, the effect of temperature in influencing fluxes was maximum in summer and minimal
#and opposite in winter. This model explained about 42% of the variation in fluxes.

library(effects)
plot(allEffects(mod_int))

#make a new dataframe
new.dat.combos <- with(df_model,
                       expand.grid(Season=unique(Season), #give me all the unique values of time
                                  Ta=seq(min(Ta),max(Ta), by=1)
                                   #give me a sequence of numbers from the min of Ta to the max of Ta, space the numbers "by 1"
                       ))

#predict fluxes using new data frame
new.dat.combos$ETp <- predict(mod_int,newdata=new.dat.combos)


###plotting prediction + data with continuous example#
ggplot(new.dat.combos,aes(x=Ta,y=ETp,colour=Season))+ #set up plot using predictions dataset
  geom_line(aes(group=Season), size = 1)+ #draw lines that are predictions, group them by light conditions
  geom_point(data=df_model, aes(x=Ta,y=ET,colour = Season), alpha = 0.2) #add the observed data to the plot

#The end!






