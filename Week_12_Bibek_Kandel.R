rm(list=ls()) # clears workspace

#resources
#https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/

##Week 12##
#This week, I am using a mixed effect model approach to test my hypothesis:
#The magnitude of the effect of net radiation on fluxes varies by seasons.

#Load the libraries
library(ggplot2)
library(tidyverse)
library(mgcv)
library(rsample)
library(gratia)
library(lubridate)
library(GGally)
library(lme4)
library(lmerTest)


set.seed(100)

#load the data
df_model = read.csv("project_data.csv")

#Create a month column
df_model$month = strftime(df_model$Date,"%m")

#Run the model
#Let's run a model with random effect intercept to each groupings (seasons)
mixed_model1 <- lmer(ET ~ 
                      #Fixed effects
                      1 +
                      #Random effects
                      (1|Season), data = df_model, REML = FALSE,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun=5e5)))

summary(mixed_model1)

#Predict and plot
df_model$pred_ET <- (fitted(mixed_model1))
PRED <-data.frame(Season=c("Autumn","Spring","Summer","Winter"), Intercepts=c(coef(mixed_model1)$Season)) 
PRED

ggplot(df_model, aes(x = Rn, y = ET)) + 
  geom_point(fill="grey", pch=21, size=1, stroke=1.25) +
  facet_wrap(~Season, ncol=5)+
  scale_y_continuous(name = "ET",limits=c(-1,1))+
  geom_abline(aes(intercept=X.Intercept., slope=0), col="red", lwd=1, PRED)

#Our results and the plot with random intercepts do not look to have a good fit...
#...suggesting possibilities for improvement. The residuals and standard deviation..
#..look pretty large.


#Let's run a model with fixed slope random intercept to each subject (Season)
mixed_model2 <- lmer(ET ~ 
                       #Fixed effects
                       1 + Rn +
                       #Random effects
                       (1|Season), data = df_model, REML = FALSE,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun=5e5)))

summary(mixed_model2)

#Predict and plot
PRED <-data.frame(Season=c("Autumn","Spring","Summer","Winter"), Intercepts=c(coef(mixed_model2)$Season[c(1,1)]),
                  Slopes=c(coef(mixed_model2)$Season[c(1 ,2)]))
PRED

ggplot(df_model, aes(x = Rn, y = ET)) + 
  geom_point(fill="grey", pch=21, size=1, stroke=1.25) +
  facet_wrap(~Season, ncol=5)+
  scale_y_continuous(name = "ET",limits=c(-1,1))+
  geom_abline(aes(intercept=Slopes..Intercept., slope=Slopes.Rn), col="red", lwd=1, PRED)

#The results from fixed slope random intercept showed a marked decrease in the residual...
#...standard deviation suggesting much better explanation of the data by allowing the...
#..model predictions to change over time.

#Let's run a model with random slope random intercept to each subject (Season)
mixed_model3 <- lmer(ET ~ 
                       #Fixed effects
                       1 + Rn +
                       #Random effects
                       (1 + Rn|Season), data = df_model, REML = FALSE,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun=5e5)))

#There were some warnings/suggestions about rescaling the predictors, however, the model..
#..converged anyway. So, I moved on given the scope of this assignment.

summary(mixed_model3)

#Predict and plot
PRED <-data.frame(Season=c("Autumn","Spring","Summer","Winter"), Intercepts=c(coef(mixed_model3)$Season[c(1,1)]),
                  Slopes=c(coef(mixed_model3)$Season[c(1 ,2)]))
PRED

ggplot(df_model, aes(x = Rn, y = ET)) + 
  geom_point(fill="grey", pch=21, size=1, stroke=1.25) +
  facet_wrap(~Season, ncol=5)+
  scale_y_continuous(name = "ET",limits=c(-1,1))+
  geom_abline(aes(intercept=Slopes..Intercept., slope=Slopes.Rn), col="red", lwd=1, PRED)


#Compare the models
anova(mixed_model1, mixed_model2, mixed_model3)

#The results showed that the random slope random intercept model has the lowest AIC..
#..suggesting that this is the best-fit model given the data. These results support my hypothesis..
#..that the magnitude of the effect of net radiation on fluxes varies among groups (Season).



















