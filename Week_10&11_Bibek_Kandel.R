rm(list=ls()) # clears workspace

##Week 10##
#This week, I am using generalized Additive Modeling (GAM) approach to test my hypothesis: 
#The rate of fluxes is driven primarily by net radiation (Rn), temperature (Ta) and Vapor... 
#...Pressure Deficit (VPD). As linear model couldn't explain much of the variation in fluxes... 
#...driven by these factors (as hypothesized earlier), generalized linear models might be a... 
#...better approach in understanding the interrelationships among them.

#Reasons for using GAMs instead of higher order polynomials:
#First, Over fitting problem and runge phenomenon leading to oversensitive model
#Second, non-linear change in the response variable over time using multiple predictors

#Resources
#https://r.qcbs.ca/workshop08/pres-en/workshop08-pres-en.html#95
#https://m-clark.github.io/generalized-additive-models/application.html#fn25
#https://environmentalcomputing.net/statistics/gams/
#https://stats.stackexchange.com/questions/234809/r-mgcv-why-do-te-and-ti-tensor-products-produce-different-surfaces

library(ggplot2)
library(tidyverse)
library(mgcv)
library(rsample)
library(gratia)
library(lubridate)
library(GGally)

set.seed(14850)

#load the data
df_model = read.csv("project_data.csv")

#Check the correlation coeffs, significance levels, distribution and scatter plots
#Caution: Takes a while to generate the plot!
#Predictors highly correlated with the response variable were selected.
GGally::ggpairs(df_model, columns = c(10:18),
                ggplot2::aes(colour = Season))


#Split the dataset
learning_split <- df_model %>%
  initial_split(prop = 0.8, strata = Season)

#Create training and testing dataset to evaluate model performance later
train <- training(learning_split)
test <- testing(learning_split)

#Create additive models
#Net radiation as a seasonal predictor
mod_add_Rn <- gam(ET ~ s(Rn) + Season, data = train, method = 'REML')

#Predict and plot
train %>%
  mutate(yhat = predict(mod_add_Rn)) %>%
  ggplot() +
  geom_point(data = train, aes(x=Rn, y=ET, color = Season), alpha = 0.05) +
  geom_line(aes(x=Rn, y=yhat, color = Season), alpha = 1.3) + ylim(-0.1, 0.5)

#Temperature as a seasonal predictor
mod_add_Ta <- gam(ET ~ s(Ta) + Season, data = train, method = 'REML')

#Predict and plot
train %>%
  mutate(yhat = predict(mod_add_Ta)) %>%
  ggplot() +
  geom_point(data = train, aes(x=Ta, y=ET, color = Season), alpha = 0.05) +
  geom_line(aes(x=Ta, y=yhat, color = Season), alpha = 1.3) + ylim(-0.1, 0.5)


#Vapor Pressure Deficit as a seasonal predictor
mod_add_VPD <- gam(ET ~ s(VPD) + Season, data = train, method = 'REML')

#Predict and plot
train %>%
  mutate(yhat = predict(mod_add_VPD)) %>%
  ggplot() +
  geom_point(data = train, aes(x=VPD, y=ET, color = Season), alpha = 0.05) +
  geom_line(aes(x=VPD, y=yhat, color = Season), alpha = 1.3) + ylim(-0.1, 0.5)


#Create interactive models
#Net radiation as a seasonal predictor
mod_int_Rn <- gam(ET ~ s(Rn, by = factor(Season)) + Season, data = train, method = 'REML')

#Predict and plot
train %>%
  mutate(yhat = predict(mod_int_Rn)) %>%
  ggplot() +
  geom_point(data = train, aes(x=Rn, y=ET, color = Season), alpha = 0.05) +
  geom_line(aes(x=Rn, y=yhat, color = Season), alpha = 1.3) + ylim(-0.1, 0.5)

#Temperature as a seasonal predictor
mod_int_Ta <- gam(ET ~ s(Ta, by = factor(Season)) + Season, data = train, method = 'REML')

#Predict and plot
train %>%
  mutate(yhat = predict(mod_int_Ta)) %>%
  ggplot() +
  geom_point(data = train, aes(x=Ta, y=ET, color = Season), alpha = 0.05) +
  geom_line(aes(x=Ta, y=yhat, color = Season), alpha = 1.3) + ylim(-0.1, 0.5)


#Vapor Pressure Deficit as a seasonal predictor
mod_int_VPD <- gam(ET ~ s(VPD, by = factor(Season)) + Season, data = train, method = 'REML')

#Predict and plot
train %>%
  mutate(yhat = predict(mod_int_VPD)) %>%
  ggplot() +
  geom_point(data = train, aes(x=VPD, y=ET, color = Season), alpha = 0.05) +
  geom_line(aes(x=VPD, y=yhat, color = Season), alpha = 1.3) + ylim(-0.1, 0.5)

#Evaluate predictive performance on training dataset
fitted_training <- train %>%
  mutate(mod_add_Rn = predict(mod_add_Rn, newdata = train),
         mod_add_Ta = predict(mod_add_Ta, newdata = train),
         mod_add_VPD = predict(mod_add_VPD, newdata = train),
         mod_int_Rn = predict(mod_int_Rn, newdata = train),
         mod_int_Ta = predict(mod_int_Ta, newdata = train),
         mod_int_VPD = predict(mod_int_VPD, newdata = train),
         dataset = "train") %>%
  pivot_longer(cols = starts_with("mod_"),
               names_to = "model",
               values_to = "yhat")


mod_p1 <- fitted_training%>%
  group_by(model, dataset) %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))


#Evaluate predictive performance on test dataset
fitted_test <- test %>%
  mutate(mod_add_Rn = predict(mod_add_Rn, newdata = test),
         mod_add_Ta = predict(mod_add_Ta, newdata = test),
         mod_add_VPD = predict(mod_add_VPD, newdata = test),
         mod_int_Rn = predict(mod_int_Rn, newdata = test),
         mod_int_Ta = predict(mod_int_Ta, newdata = test),
         mod_int_VPD = predict(mod_int_VPD, newdata = test),
         dataset = "test") %>%
         pivot_longer(cols = starts_with("mod_"),
               names_to = "model",
               values_to = "yhat")


mod_p2 <- fitted_test%>%
  group_by(model, dataset) %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))

#Plot a histogram of model performance on training and testing dataset
mod_perform <- bind_rows(mod_p1, mod_p2)

#Relevel the dataset column and plot
mod_perform$dataset <- relevel(factor(mod_perform$dataset), ref = "train")
ggplot(data=mod_perform, aes(x=model, y=mse, fill=dataset)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


#Our results suggested that the interactive models generally perform better as predictors..
#..of fluxes performed relatively better (mean squared error = 0.006 for training and 0.007 for..
#..testing dataset) compared to other models. 

#Generalized additive model using multiple variables as predictors
mod_gam2 <- gam(ET ~ s(Rn) + s(Ta) + s(VPD) + s(H) + Season, data = train, method = 'REML')
summary(mod_gam2)
vis.gam(mod_gam2, theta = 45)

#Model diagnostics
appraise(mod_gam2, method = "simulate")

#Plot the graph
plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)
gratia::draw(mod_gam2)

#Model comparison with linear model
mod_lm2 <- lm(ET ~ Rn + Ta + VPD + H + Season, data = train)
summary(mod_lm2)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(mod_lm2)

#Model comparison
anova(mod_lm2, mod_gam2, test = "F")
AIC(mod_lm2, mod_gam2)

#The model comparison results showed additional statistical evidence to suggest that...
#incorporating nonlinear relationships of the features improve the model and explains greater...
#...variation in fluxes compared to that of linear model.

#Evaluate predictive performance on test dataset between linear model and GAM
#linear
fitted_test2 <- test %>%
  mutate(yhat = predict(mod_lm2, newdata = test),
         dataset = "test")

Error_LM <- fitted_test2 %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))

#GAM
fitted_test3 <- test %>%
  mutate(yhat = predict(mod_gam2, newdata = test),
         dataset = "test")

Error_GAM <- fitted_test3 %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))

Error_LM
Error_GAM

#This reinforces the conclusion that GAM performs better than linear model at predicting flux...
#...values using interactive relationship among predictors.


#STEPWISE GAM
#Create a model
test_model1 <- gam(ET ~ as.factor(Season) + s(Rn), data = train, method = "REML")
summary_1 <- summary(test_model1)
summary_1

#par(mar=c(1,1,1,1)) #To change the plot configuration

#Qualitative assessment of model fit using plots
par(mfrow = c(2,2))
plot(test_model1, all.terms = TRUE)

#Quantitative assessment of model fit
summary_1$p.table
summary_1$s.table

#EDF is higher than one meaning the spline is more wiggly and non-linear.
#Lower EDF means the model is more penalized and tends to be more linear.

#Add more predictors
test_model2 <- gam(ET ~ as.factor(Season) + s(Rn) + s(VPD), data = train, method = "REML")
summary_2 <- summary(test_model2)
summary_2

#Qualitative assessment of model fit using plots
par(mfrow = c(2,2))
plot(test_model2, all.terms = TRUE)

#Quantitative assessment of model fit
summary_2$p.table
summary_2$s.table

#model comparisons using AIC and Likelihood ratio tests
AIC(test_model1, test_model2)
anova(test_model1, test_model2, test = "F")
#Result showed test_model_2 is a better fit. Should we add more predictors? Let's check!

#Add more predictors
test_model3 <- gam(ET ~ as.factor(Season) + s(Rn) + s(VPD) + s(Ta), data = train, method = "REML")
summary_3 <- summary(test_model3)
summary_3

#Qualitative assessment of model fit using plots
par(mfrow = c(2,2))
plot(test_model3, all.terms = TRUE)

#Quantitative assessment of model fit
summary_3$p.table
summary_3$s.table

#model comparisons using AIC and Likelihood ratio tests
AIC(test_model2, test_model3)
anova(test_model2, test_model3, test = "F")
#Results showed adding smoother function of temperature as a predictor increases model fit!
#Let's check for one more predictor that was not in our hypothesis: H

#Add more predictors
test_model4 <- gam(ET ~ as.factor(Season) + s(Rn) + s(VPD) + s(Ta) + s(H), data = train, method = "REML")
summary_4 <- summary(test_model4)
summary_4

#Qualitative assessment of model fit using plots
par(mfrow = c(2,2))
plot(test_model4, all.terms = TRUE)

#Quantitative assessment of model fit
summary_4$p.table
summary_4$s.table

#Model comparison
AIC(test_model3, test_model4)
anova(test_model3, test_model4, test = "F")
#Results showed that both tests agree test_model_4 is the best model fit among them.



#INTERACTION EFFECTS
test_model_int <- gam(ET ~ as.factor(Season) + s(Rn, by=factor(Season)) + s(Ta, by=factor(Season)) + 
                        s(VPD, by=factor(Season)) + s(H, by=factor(Season)), data = train, method = "REML")
summary(test_model_int)

#Qualitative assessment of model fit using plots
#plot(test_model_int, all.terms = TRUE)

#Quantitative assessment of model fit
summary_4$p.table
summary_4$s.table

#Plot interaction effect on 3D
par(mfrow = c(1,1))
vis.gam(test_model_int, theta = 45, n.grid = 50, lwd = 0.2)

#Error
#GAM
fitted_test3 <- test %>%
  mutate(yhat = predict(test_model_int, newdata = test),
         dataset = "test")

Error_GAM <- fitted_test3 %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))

#Model comparisons using likelihood ratio test for GAMs
anova(test_model4, test_model_int, test = "F")
AIC(test_model4, test_model_int)

#Both test results showed that the interaction model is a better fit model to the given..
#..data and the result is statistically clear (p<0.001).

#Compare the predictive errors between linear and interactive non-linear models
Error_LM
Error_GAM

#Additional considerations
#Is our model wiggly enough? Check k value...
#If the k-value and EDF are similar, your model could do better!
k.check(test_model_int)

#Since the distribution of residuals is non-normal, using a different distribution
#Until now, we were using Gaussian distribution with identity link.
#Now, we'll use tweedie distribution with log link.
train <- train %>% filter(ET > 0)

test_tw_int <- gam(ET ~ as.factor(Season) + s(Rn, by=factor(Season)) + s(Ta, by=factor(Season)) + 
                        s(VPD, by=factor(Season)) + s(H, by=factor(Season)), 
                        family = tw(link = "log"), data = train, method = "REML")
summary(test_tw_int)

k.check(test_tw_int)

#Check diagnostic plots again!!!
par(mfrow = c(2,2))
gam.check(test_tw_int)

#Compare models with Gaussian and tweedie distributions
AIC(test_tw_int, test_model_int)

#Error
#GAM
fitted_test3 <- test %>%
  mutate(yhat = predict(test_tw_int, newdata = test),
         dataset = "test")

Error_GAM <- fitted_test3 %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))

#Compare the predictive errors between linear and interactive non-linear models
Error_LM
Error_GAM

#NOTE: Using a tweedie distribution improved the model fit in terms of AIC, however,
#The predictive performance of the model decreased substantially on test dataset. 


#SMOOTH FUNCTIONS FOR MULTIDIMENSIONAL INTERACTIONS WITH DIFFERENT SCALES
#In the example below, we are using a type of smooth called a tensor product smooth, and...
#...by smoothing the marginal smooths of predictors, we see a bit clearer story. Tensor product..
#..smooths address the issue of modeling responses to interactions of multiple inputs with different units.

mod_gam = mgcv::gam(ET ~ Season + te(Ta, VPD, Rn, H, d=c(1,1,2)), data = train)
summary(mod_gam)

k.check(mod_gam)

#Model diagnostics
appraise(mod_gam, method = "simulate")

#Plot the graph
par(mfrow = c(1,2))
vis.gam(mod_gam, view=c("Rn","Ta"), theta = 45, n.grid = 50, lwd = 0.2)
vis.gam(mod_gam, view=c("VPD","H"), theta = 45, n.grid = 50, lwd = 0.2)

#Model comparisons among additive and interactive
AIC(mod_lm2, test_model4, test_model_int, mod_gam)

#Error
#GAM
fitted_test3 <- test %>%
  mutate(yhat = predict(mod_gam, newdata = test),
         dataset = "test")

Error_GAM <- fitted_test3 %>%
  mutate(error = ET - yhat) %>%
  mutate(sq_error = error ^2) %>%
  summarize(mse = mean(sq_error))

#Compare the predictive errors between linear and interactive non-linear models
Error_LM
Error_GAM

#Change date format
fitted_test3$newdate <- strptime(as.character(fitted_test3$DateTime), "%m/%d/%Y")

#Predict and plot
fitted_test3 <- fitted_test3 %>%
  group_by(newdate) %>%
  summarize(yhat_mean = mean(yhat),
            ET_mean = mean(ET),
            temp = mean(Ta),
            upper = quantile(yhat, 0.975),
            lower = quantile(yhat, 0.025))

library(ggbreak)
ggplot(fitted_test3, aes(x = as.Date(newdate))) +
  geom_point(aes(y = ET_mean), colour = "blue", alpha=0.5) +
  geom_line(aes(y = yhat_mean), colour = "red",linewidth = 0.9) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  labs(title = "Time series of predicted fluxes using Generalized Additive Models
(Data available only for 2017 and 2021)", x = "Year-month", y = "Flux (umol/m2.s)") +
  ggbreak::scale_x_break(c(ymd("2017-12-25"), ymd("2021-01-24")))+
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  #geom_line(aes(y = temp), colour = "purple",linewidth = 0.9) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name="Temperature (degC)"))+
  theme_bw()
  #geom_hline(yintercept=0.21, linetype="dashed", color = "black", size=1)






