# lm 16.09.15

# load required packages
library(hadleyverse)
library(foreign)
library(haven) # assuming stata13 files
library(checkpoint)
library(ProjectTemplate)
library(explainr)
library(magrittr)
library(psych)
library(MASS)
library(arm)
library(broom)


# set wd
setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")

# read in data
Males<-read_dta("Males.dta")
summary(Males)
View(Males)


# Visualize dataset
pairs(Males)
## This error can occur in Rstudio simply because your "Plots" pane is just barely too small. Try zooming your "Files, Plots, Packages, Help, Viewer" and see if it helps!


# Histogram 
discrete.histogram(Males$SCHOOL)
hist(Males$SCHOOL)

## or via ggplot2
m <- ggplot(Males, aes(x=SCHOOL))
m + geom_histogram(aes(y = ..density..),colour = "darkgreen", fill = "white",binwidth = 1)
m + stat_function(fun = dnorm, aes(colour = 'Normal'))   
# m + geom_histogram(colour = "darkgreen", fill = "white",binwidth = 1) + geom_density()

Wage<-Males$WAGE
Males$ExpWage      <- exp(Wage)
View(Males)


hist(Males$WAGE)
hist(Males$ExpWage)

w0 = ggplot(Males, aes(x=Wage), geom = "blank") +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4) +  
  labs(title="Histogram for Wage") 
  scale_colour_manual(name = 'Density', values = c('red', 'blue'))
print(w0)


w1 = ggplot(Males, aes(x=ExpWage), geom = "blank") +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4) +  
  labs(title="Histogram for Expo Wage") 
  scale_colour_manual(name = 'Density', values = c('red', 'blue'))
print(w1)


s0 = ggplot(Males, aes(x=SCHOOL), geom = "blank") +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4,binwidth=1) +  
  labs(title="Histogram for School") + 
    scale_colour_manual(name = 'Density', values = c('red', 'blue'))
print(s0)

# estimnate OLS regression 
names(Males)
lm1<- lm(WAGE ~ EXPER + SCHOOL + UNION + MAR + BLACK, data=Males) 
summary (lm1)
## output is correct

# extract coefficients
lm1.tidy<-tidy(lm1)
lm1.coef<-lm1.tidy$estimate

# For precise effect size use exp(a)~1+a i.e. a=exp(a)-1
# we replace the parameter estimate with the exponant -1 to get the more exact effect
# calulate exact the log and the exp function
# the exponent of the exact calculation should be larger


