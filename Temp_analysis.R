# Temp Data Analysis


setwd("C:/Users/Rachelle/Documents/RProjects/2020Rice_Growth_Data/data_output")
# 2020 River Garden Farms Rice Field Temperature and DO Data


library(tidyverse)
library(nlme) # used for gls model
library(tseries) # used for ARIMA model
library(forecast) # used for ARIMA model

library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(ggborderline)
library(gridExtra)

temp_dat <- read_csv("temp_combined.csv")

temp_dat$Plot <- as.factor(temp_dat$Plot)

temp_dat$Hab_type <- as.factor(temp_dat$Hab_type)

temp_dat$Hab_type <- factor(temp_dat$Hab_type, levels = c("Control", "Canals", "Wood", "Wood & Canals"))

temp_dat2 <- temp_dat %>% 
  filter(Date_time >"2020-02-06" & Date_time <"2020-03-02")



# Generalized least squares -----------------------------------------------

# type of linear model
# provides for correlations
# can be applies to a time series regression

# only use on stationary data
# select the correct correlation structure
# we are using hourly data, Jon suggested using AR(2) - which is based on previous periods not previous error. This produces cycles which we need daily and night temperature observations


# first re-order the data so all the habitat treatments are compared to the control treatment values




plot(Temp ~ Date_time, type="n", data= temp_dat,
     ylab="Hourly Temperatures") +
  abline()+
grid(lty=1)
with(temp_dat, points(Date_time, Temp, type="o", pch=16, col= Plot)) 

# Plots clearly overlap one another
# data is definitely autocorrelated

# Run the Generalized Least Squares Model

mod.gls <- gls(Temp ~ Date_time + Hab_type,
               data=temp_dat2, correlation=corARMA(p=2), method="ML")


summary(mod.gls)

# Results

# Standardized residuals:
#  Min          Q1         Med          Q3         Max 
# -3.30322215 -0.67994855  0.05195995  0.73990534  2.73753145 
# Residual standard error: 3.386492 
# Degrees of freedom: 5234 total; 5231 residual

# mod.gls.3 <- update(mod.gls, correlation=corARMA(p=3))

# Generalized least squares fit by maximum likelihood
# Model: Temp ~ Date_time + Hab_type 
# Data: temp_dat2 
# AIC      BIC    logLik
# 1860.283 1911.012 -922.1413

# Correlation Structure: ARMA(2,0)
# Formula: ~1 
# Parameter estimate(s):
#  Phi1       Phi2 
# 1.8541135 -0.9009239 

#Coefficients:
#  Value Std.Error   t-value p-value
# (Intercept)           -481.2314  68.41294 -7.034216  0.0000
# Date_time                0.0000   0.00000  7.224310  0.0000
# Hab_typeCanals          -0.1276   0.14533 -0.877963  0.3800
# Hab_typeWood            -0.1785   0.12545 -1.422609  0.1549
# Hab_typeWood & Canals   -0.1559   0.17766 -0.877692  0.3802

# Correlation: 
#  (Intr) Dat_tm Hb_tyC Hb_tyW
# Date_time             -1.000                     
# Hab_typeCanals        -0.243  0.242              
# Hab_typeWood          -0.124  0.123  0.510       
# Hab_typeWood & Canals -0.132  0.131  0.541  0.516

# Standardized residuals:
#  Min           Q1          Med           Q3          Max 
# -2.562853081 -0.681472330 -0.002463861  0.691401419  3.261224411 

# Residual standard error: 3.147493 
# Degrees of freedom: 4193 total; 4188 residual






# Ignore ------------------------------------------------------------------

# Create a time series object

temp_ts <- ts(temp_wide,start=c(2020,02-05),frequency = 5) # tells when the time series starts 02-05-2020 and the interval (5 weeks)

temp_ts <- ts(temp_wide, start=c(2020,02-05),frequency = 8760) # tells when the time series starts 02-05-2020 and the interval (5 weeks)

is.ts(temp_ts)



temp_dat$Hab_type <- as.factor(temp_dat$Hab_type)




# ARIMA Model Analysis ----------------------------------------------------

# check for stationarity

plot.ts(temp_dat$Temp) # -> not much of a trend (up or down trend)


# use an adf test to test whether data is stationary or not stationary

adf.test(temp_dat$Temp)

# calculate p-value 0.01 <0.05. 


# Data is stationary



# Time Series Analysis ----------------------------------------------------

# Best fit ARIMA model

auto.arima(temp_dat$Temp)

# Best fit model ARIMA(4,1,1) 

# AR Lage of 4
# Stationary at 1
# Residue lag of 1


# Create a model

model_temp <- arima(temp_dat$Temp, order = c(4,1,1))

model_temp

# Model equation
# temp = -0.9129 + 1.3360temp(t-1) + (-0.1596)temp(t-2) + (-0.1916)temp(t-3) + (-.1088)temp(t-4) + (-0.1088)et-1




# Diagnostic Check --------------------------------------------------------


# Calculate Residual parameter

et <- residuals(model_temp)


# Correlation
acf(et)

# mine are autocorrelated... because it's temperature

plot.ts(et)
# residual means hover around 0

# Check if the data normally distributed
gghistogram(et)
# bell shaped- mostly normally distributed -> but super narrow


Box.test(et, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 5) #fitdf = AR + Residue lag values

# H0 residuals do not follow IID (residuals are not independent)


# Generalized Additive Models GAM  --------------------------------------------------------------------

# between a simple and more complex models

library(gam)

temp_dat$Plot <- as.factor(temp_dat$Plot)

gam_mode <- gam(Temp ~ s(Date_time, by= Plot), data = temp_dat)
gam(temp ~ s(time, by=plot))
gam_mod <-gam(Temp ~ s(Date_time + Plot), temp_dat, family = gaussian)


colnames(temp_dat)




# ARMA Model --------------------------------------------------------------

install.packages("nlme")
library(nlme)

# make the data long
temp_wide <- temp_dat %>% 
  select(-c("#", "DO", "Hab_type")) %>%  # remove unique row identifier
  pivot_wider(names_from = "Plot", values_from = "Temp")

temp2 <- temp_dat %>% 
  select(Date_time,Temp, Plot)

plot(temp2$Date_time,temp2$Temp,col = factor(temp2$Plot))


# Look at auto correlation
temp3 = groupedData(Temp~as.numeric(Date_time)|Plot,data=temp2)

plot(temp3)


# mod 1

mod1 = gls(Temp ~ Date_time,data = temp2, method="ML") # method= ML stands for maximum likelihood
summary(mod1)
# p-values are bogus because we know the temperatures are autocorrelated


# determine if data is autocorrelated 
acf(residuals(mod1), lag.max=10, plot=FALSE) # data is super auto  correlated...

# Account for auto correlation in the model

mod2 = gls(Temp ~ Date_time, data= temp2 ,method="ML",correlation=corAR1())
summary(mod2)

# AIC value has been reduced

# Using corARMA
arma2 = corARMA(c(.2,.2),p=2,q=0)


# Create a model including auto regressive and moving correlation
mod3 = gls(Temp ~ Date_time,data= temp2,method="ML",correlation=arma2)
summary(mod3)


