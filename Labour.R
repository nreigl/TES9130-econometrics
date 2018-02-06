# load required packages
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
library(AER)
library(gmm) # Generalized Method of Moments and Generalized Empirical Likelihood
library(aod)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(boot)
library(memisc)
library(stargazer)
library(plm)
library(wfe)
library(lmtest)  # for waldtest()
library(car)
library(reshape2)
library(data.table)

# read in data
labour <-read_dta("Labour.dta")
dim(labour)
summary(labour)
View(labour)      ## dummy years are already generated.

## Declare the data as panel-data
labour <- plm.data(labour, index=c("id","year"))
head(labour)



coeftest

# Pooled OLS estimator
mod1 <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, data=labour, model= "pooling")
#options(digits = 6)  # for more exact comparison with Stata's output but nothing changed
coeftest(mod1)
summary(mod1)          ## SAME RESULT AS IN STATA


?vcovHC
test1<-coeftest(vcovHC(mod1, method =c("white1"), type =c("HC1")))

str(test1)








# Wrong White heteroskesdastic-consistent standard errors 
#mod2 <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, data=labour, model= "pooling")
hc1<-coeftest(mod1, vcov = vcovHC(mod1, type = "HC1"))   ## Results different from STATA
hc2<-coeftest(mod1, vcov = vcovHC(mod1, type = "HC2"))   ## Results different from STATA
hc3<-coeftest(mod1, vcov = vcovHC(mod1, type = "HC3"))   ## Results different from STATA
hc4<-coeftest(mod1, vcov = vcovHC(mod1, type = "HC4"))   ## Results different from STATA

hc1
hc2
hc3
hc4


?coeftest


#Correct panel robust standard errors
G <- length(unique(labour$id))
N <- length(labour$id)
dfa <- (G/(G - 1)) * (N - 1)/mod1$df.residual
# display with cluster VCE and df-adjustment
#mod3 <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, data=labour, model= "pooling")
firm_c_vcov <- dfa * vcovHC(mod1, type = "HC0", cluster = "group", adjust = T)
coeftest(mod1, vcov = firm_c_vcov)   # Same Results as in STATA


#summary(coeftest(mod3))  If I run this line, I get different t value. Not 45,98 but 89,12 that it is wrong!



## Standard error using iid errors and in some cases panel (make table)
stargazer(mod1, mod2, mod3, title="Panel regressions, clustered SEs", type="text", 
          column.labels=c("Pooled OLS", "White", "Cluster"), df=FALSE, digits=4)
## Set the regressions above and the lines above for table works!



##Correct panel bootstrap standard errors      
## No idea



# Scatter plot Overall plot of data with lowess local regression line 
library(ggplot2)
ggplot(data=labour, aes(x=lnwg, y=lnhr)) +
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Pooled Overall Regression", x="Log hourly wage", y="Log annual hours")   ## THis plot is ok.
save.image("d:/pooled regression.RData")



#Betweeen plot of data with lowess local regression line
remove(mod1, mod2, mod3, labour, dfa, G, N, firm_c_vcov)
# read in data
labour <-read_dta("Labour.dta")

## Declare the data as panel-data
labour <- plm.data(labour, index=c("id","year"))
head(labour)



#Between effects
re <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, data=labour, index=c("id","year"),  effect="individual",model= "random")
summary(re)
library(ggplot2)
ggplot(data=labour, aes(x=lnwg, y=lnhr)) +
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Between", x="Log hourly wage", y="Log annual hours")   
save.image("d:/between.RData")



#Within plot of data with lowess local regression line
remove(re)
# read in data
labour <-read_dta("Labour.dta")

## Declare the data as panel-data
labour <- plm.data(labour, index=c("id","year"))
head(labour)

# Fixed effect or within estimator
fixed <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, data=labour, model= "within")
summary(fixed)    
library(ggplot2)
ggplot(data=labour, aes(x=lnwg, y=lnhr)) +
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Within", x="Log hourly wage", y="Log annual hours")   
save.image("d:/between.RData")


#The following only works if each observation is (i,t) and within i the data are ordered by t
remove(fixed, dfa,G,N, firm_c_vcov)
# read in data
labour <-read_dta("Labour.dta")

## Declare the data as panel-data
labour <- plm.data(labour, index=c("id","year"))
head(labour)


## Generate dvariable as difference between years. An alternative approach to taking advantage of the 
#full panel is to run the regression in first differences. That is, we create new variables 
#that represent the year-to-year change in each variable
mod4 <- plm(lnhr ~ lnwg + kids + ageh + agesq + disab, data=labour, effect="individual", model= "fd")

# It generates in wrong way the covariates. Not dlnhr, dlnwg but dyear1 and so on. 

# Scatter plot 
library(ggplot2)
ggplot(data=labour, aes(x=dlnhr, y=dlwg)) +
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="First Difference", x="dlnwg", y="dlnhr")     
save.image("d:/fd.RData")



## Combine all 4 plots together


#The following only works if each observation is (i,t) and within i the data are ordered by t
remove(labour, mod4)

# //**** COMPARE PANEL DATA ESTIMATORS *** ///
# read in data
labour <-read_dta("Labour.dta")

## Declare the data as panel-data
labour <- plm.data(labour, index=c("id","year"))
head(labour)



# Correct panel robust standard errors

# Usual standard errors assume iid error

# STATA has no option for correct panel robust standard errors, bootstrapped s.e. must be calculated



#################################################* *//////////////////////////////////////////////////////
#

#
#################################################* *//////////////////////////////////////////////////////
library(lmtest)
library(car)
data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, model = "random")
## standard coefficient significance test
coeftest(zz)
## robust significance test, cluster by group
## (robust vs. serial correlation)
coeftest(zz, vcov=vcovHC)
## idem with parameters, pass vcov as a function argument
coeftest(zz, vcov=function(x) vcovHC(x, method="arellano", type="HC1"))
## idem, cluster by time period
## (robust vs. cross-sectional correlation)
coeftest(zz, vcov=function(x) vcovHC(x, method="arellano",
                                     type="HC1", cluster="group"))
## idem with parameters, pass vcov as a matrix argument
coeftest(zz, vcov=vcovHC(zz, method="arellano", type="HC1"))
## joint restriction test
waldtest(zz, update(zz, .~.-log(emp)-unemp), vcov=vcovHC)
## test of hyp.: 2*log(pc)=log(emp)
linearHypothesis(zz, "2*log(pc)=log(emp)", vcov=vcovHC)


