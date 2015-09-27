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


# Exp Wage
Wage<-Males$WAGE
Males$ExpWage<- exp(Wage)

hist(Males$WAGE)
hist(Males$ExpWage)

## or via ggplot2
# Hist. Wage
w0 = ggplot(Males, aes(x=Wage), geom = "blank") +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4) +  
  labs(title="Histogram for Wage") 
  scale_colour_manual(name = 'Density', values = c('red', 'blue'))
print(w0)

# Hist. Exp Wage
w1 = ggplot(Males, aes(x=ExpWage), geom = "blank") +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4) +  
  labs(title="Histogram for Expo Wage") 
  scale_colour_manual(name = 'Density', values = c('red', 'blue'))
print(w1)


# Hist School
s0 = ggplot(Males, aes(x=SCHOOL), geom = "blank") +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4,binwidth=1) +  
  labs(title="Histogram for School") + 
    scale_colour_manual(name = 'Density', values = c('red', 'blue'))
print(s0)
## discrete variables influence the distribution function
## proposed workarounds: 1) transform variables or 2) play with bin_geom 3) use lattice to plot it


# estimnate OLS regression 
names(Males)
lm1<- lm(WAGE ~ EXPER + SCHOOL + UNION + MAR + BLACK, data=Males) 
summary (lm1)
## output is correct

# For precise effect size use exp(a)~1+a i.e. a=exp(a)-1
# we replace the parameter estimate with the exponant -1 to get the more exact effect
# calulate exact the log and the exp function
# the exponent of the exact calculation should be larger
lm1.exactlog<-exp(lm1$coefficients)-1

# row bind the approximation and the exact log beta_hat
beta_hat.comparison<-rbind(lm1$coefficients, lm1.exactlog)
beta_hat.comparison

# III GENERATE WAGE PREDICTION IN LEVELS - THE RETRANSFORMATION PROBLEM ON LOG-LINEAR MODEL //
# Note that exp(E(ln y) is not equal to E(y), BUT E(y|x)=exp(x'b)E(exp(u))
# 1) Assuming the error term, u, is normally distributed u=N(0, sigma^2), then E(y|x)=exp(x'b)exp(0.5 s^2)
# find exact number in levels (in Dollar) not in log Dollars

# "old way"
lyhat_mean<-mean(fitted.values(lm1))
lyhat_sd<-sd(fitted.values(lm1))
lyhat_min<-min(fitted.values(lm1))
lyhat_max<-max(fitted.values(lm1))
cbind(lyhat_mean, lyhat_sd, lyhat_min, lyhat_max)

# dplyr way 
df<- as.data.frame(predict.lm(lm1)) 
names(df)[names(df)=="predict.lm(lm1)"] <- "lnyhat" # rename columnnam

lnyhat.df <-df %>%
  summarise(length(lnyhat),
            mean = mean(lnyhat), 
            st.dev. = sd(lnyhat),
            min = min(lnyhat),
            max = max (lnyhat))
            

# assuming normally distributed error terms we can have 0.5. should work over the predict function in R
rse<-summary(lm1)$sigma # extract residual standard error for simplicity
yhatnormal<-exp(lnyhat.df$mean)*exp((0.5)*rse^2)
yhatnormal
## not exactly the same results as stata!

# 2) Duan(1983) approach: apply a weaker assumption of iid error term and estimate E(exp(u))=avg(exp(u_hat)), then E(y|x)=exp(x'b)avg(exp(u_hat))
# predict residuals u_ha
lnyhat = predict(lm1, data.frame(Males)) ## update as the code line was missing
lnyhat
summary(lnyhat)

e<-(lm1$residuals)
summary(e)
Duan <- (exp(lm1$residuals))
summary(Duan)                 ## Same results as in STATA

yhat_Duan<- (exp(lnyhat)*mean(lnyhat))
summary(yhat_Duan)            ## Not same results as in STATA

### For comparison generate the wrong prediction in levels using exp(E(ln y)), generate the WAGE in levels and compare with the results above
yhat_wrong<- exp(lnyhat)  
summary(yhat_wrong)           ## Same results as in STATA
summary(WAGE_lev)
summary(yhatnormal)
summary(yhat_Duan)
summary(yhat_wrong)


### IV ESTIMATE OLS WITH INTERACTION BETWEEN SCHOOLING AND MINORITY 
MINORITY<-ifelse(Males$BLACK==1 | Males$HISP==1, 1,0)
summary(MINORITY)
MINSCH<-Males$SCHOOL*MINORITY
summary(MINSCH)          ## Same results like in STATA

# Generate regression with interaction MINORITY*SCHOOL

##  xi: gives me interaction term and the respective dummyies
## regress WAGE EXPER UNION MAR i.MINORITY*SCHOOL 

names(Males)
lm2<- lm(WAGE ~ EXPER + UNION + MAR + BLACK, data=Males) 
summary(lm2)


############################################
# Last commit ##############################
############################################


