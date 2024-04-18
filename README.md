# Kandel_QMEE
This repo contains all the information required for Quantitative methods in Ecology and Evolution.

<p> ##Week 1 Assignment
<p> Write a paragraph about your data describing your goals for the semester.

<p> This dataset contains two years’ worth of high-frequency (10 Hz) carbon/water flux data with a suite of meteorological data collected using the Eddy Covariance method from the Falling Creek reservoir in Venton, Virginia. Carbon dioxide and methane are the two major green-house gases exchanged across freshwater-atmosphere interface that contribute to the warming of atmosphere globally through heat trapping mechanisms in the troposphere. Thus, long-term monitoring and exploration of the drivers of these fluxes can address some of the big questions: (a) How do small freshwater reservoirs respond to changing climate in terms of gas exchange? (b) Can we predict the carbon fluxes in the near future? (c) How can we better manage these natural resources for a sustainable reservoir ecosystem (e.g., aerated future)? My goal for one of the dissertation chapters would be to answer these questions by: (1) the characterization of seasonal dynamics of the gas exchange as a proxy of reservoir’s biological activities, and (2) the analysis of drivers of carbon fluxes as an understanding of their responses to changing climate.


<p> #Week 2 Assignment
<p> CODE : Week2_Kandel_QMEE
<p> DATA: project_data.csv

Here, I practiced tidying the format of my data. I used group_by to take a mean of ET across seasons and also used mutate function to create an additional column to calculate ET from LE.

#The end!

<p> #Week 3 Assignment
<p> CODE: Week3_Bibek_Kandel
<p> DATA: project_data.csv

This week, I tried to answer some of the fundamental questions about land-water-atmosphere water flux dynamics through data visualization using ggplot in R. These visualizations should help understand the preliminary relationships among flux and their drivers at various temporal scales.

#The end!


<p> #Week 5 Assignment
<p> CODE: Week5_Bibek_Kandel
<p> DATA: project_data.csv

This week, I am going to formulate some hypotheses and test them against my project data using a mixture of classical and permutation test. Firstly, I hypothesized that my data is normally distributed. Second, the mean ET for the summer season is significantly higher than that for the spring season. The mean annual evapotranspiration for the year 2021 is significantly higher than that for 2017 owing to the increase in temperature and vapor pressure deficit following global warming. The increase in temperature and vapor pressure deficit is generally thought to promote the rate of evaporation in light of global warming.

#The end!


<p> #Week 7 & 8 Assignment
<p> CODE: Week_7&8_Bibek_Kandel
<p> DATA: project_data.csv

This week, I am going to further investigate into the interesting results obtained in last week's analysis. Given the results showed statistically clear decline in the annual rate of ET, I am developing a uni-variate linear model using only net radiation and then other variables to test the hypothesis: The rate of fluxes is driven primarily by net radiation (Rn), temperature (Ta) and Vapor Pressure Deficit (VPD). The findings will help us understand the ecosystem response to changing meteorological conditions, ultimately contributing to better understand and manage natural resources in light of climate change. 

#The end!

<p> #Week 10 Assignment
<p> CODE: Week_10&11_Bibek_Kandel
<p> DATA: project_data.csv 

This week, I am using generalized Additive Modeling (GAM) approach to test my hypothesis: The rate of fluxes is driven primarily by net radiation (Rn), temperature (Ta) and Vapor Pressure Deficit (VPD). As linear model couldn't explain much of the variation in fluxes driven by these factors (as hypothesized earlier), generalized linear models might be a better approach in understanding the interrelationships among them. 

#The end!

<p> #Week 11 Assignment
<p> CODE: Week_10&11_Bibek_Kandel
<p> DATA: project_data.csv 

This week, I am using various model comparison and selection approaches to identify and select the best-fit models, given the data. I'll use Akaike's Information Criteria (AIC) and Likelihood Ratio Tests (LRT) approach to make decisions about the best-fit model. Also, I'll assess the model predictive performance to supplement the selection process. 

#The end!

<p> #Week 12 Assignment
<p> CODE: Week_12_Bibek_Kandel
<p> DATA: project_data.csv 

This week, I am using mixed effects modeling approach to evaluate how the effects of net radiation on fluxes vary over seasons. I'll use Akaike's Information Criteria (AIC) approach to make decisions about the best-fit model. Also, I'll assess the model predictive performance to assess the model fit.

#The end!









