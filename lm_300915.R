# lm_300915

# load required packages
library(hadleyverse)
library(foreign)
library(haven) # assuming stata13 files
library(checkpoint)
library(ProjectTemplate)
library(explainr)
library(magrittry
        library(psych)
        library(MASS)
        library(arm)
        library(broom)
        library(gmm) # Generalized Method of Moments and Generalized Empirical Likelihood
        
        
        # set wd
        setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")
        
# read in data
Schooling<-read_dta("Schooling_dta.dta")
summary(Schooling)
View(Schooling)
        
        
        