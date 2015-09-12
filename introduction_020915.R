## session 1: 02.09.2015


# load required packages
library(hadleyverse)
library(foreign)
library(haven) # assuming stata13 files
library(checkpoint)
library(ProjectTemplate)
library(explainr)
library(magrittr)
library(psych)


# set WD
setwd("~/studium/TUT/TES9130 econometric (phd)/R/TES9130_econometrics")

# create project template (optional)
create.project('TES9130', merge.strategy = ("allow.non.conflict"))
# create checkpoint (optional)
# checkpoint("2015-09-02")

# load .dta file
Cars<-read_dta("DatasetCars.dta")

# descriptive statistics
summary(Cars)
View(Cars)



# Find the mean weight of cars E(y)

mean(Cars$weight)
length(Cars$rep78)

# replace empty character strings with R typical NA
Cars$rep78[Cars$rep78 == ""]<-NA

# Create filtered list with NA observations excluded 
Cars.f<-Cars %>%
  filter(!is.na(Cars$rep78))

# Number of obs. 
length(Cars.f$rep78)
summary(Cars.f)

# Find the mean weight of cars.f E(y)
mean(Cars.f$weight)
t.test(Cars.f$weight) # gives us the 95% conf.  intervals  (CI)

# Find the geometric mean of cars.f E(y)
geometric.mean(Cars.f$weight)
## NB: no auto conf. function but easy to calculate by yourself
ci.gm<-function(x){
  gm1 = mean(log(x), na.rm = T)
  cil = exp(gm1-(1.96*(sd(log(x), na.rm = T)/sqrt(length(x))))) # 1.96 is approximated
  ciupp = exp(gm1+(1.96*(sd(log(x), na.rm = T)/sqrt(length(x)))))
  vec = c(round(cil,2), round(ciupp,2))
  return (vec)
}
ci.gm(Cars.f$weight)

# Find the harmonic mean of cars.f E(y)
harmonic.mean(Cars.f$weight)

# Find E(E(weight|rep78))
Cars.f$rep78 <- as.character(Cars.f$rep78)
names(Cars.f$rep78)
## the names function gives me a Null vector. Possible solutions:
# convert character to factor
Cars.f$rep78fac<-as.factor(Cars.f$rep78)
levels(Cars.f$rep78fac)
## working!

# first find total number of observations
N<-Cars.f %>% 
  summarise(length(rep78))
N


# Calculate mean of weights, sd, min. weight, max. weight and number of observations per quality class
Cars.f %>%
   group_by(rep78) %>% 
  summarise(length(rep78),
            avg = mean(weight),
            local.avg = (avg*length(rep78)/(69)), # dirty solution (N=69 as can be seen by the previous function): surprisingly I did not find a way to ungroup and then sum up the length of the vector. Defining N, even though the result is saved, does not work: output for local avg is <dbl[1]>
            weight.max = max(weight),
            weight.min = min(weight))



# 
# Calculate mean of weights, sd, min. weight, max. weight and number of observations per quality class for domestic cars
Cars.f %>%
  filter(foreign==0) %>% 
  summarise(length(rep78),
            avg = mean(weight),
            sd(weight),
            weight.max = max(weight),
            weight.min = min(weight))  


# Prove that E(E(weight| rep78, foreign) | rep78)=E(weight | rep78)
E1<-as.data.frame((Cars$foreign)^2*Cars$weight)
colnames(E1) <- letters[ncol(E1)] # define colname dynamically

E1 %>% 
  summarise(length(a),
            avg = mean(a, na.rm = TRUE),
            sd(a),
            weight.max = max(a),
            weight.min = min(a)) 


# levels of foreign
Cars$foreign.fac<-as.factor(Cars$foreign)
levels(Cars$foreign.fac)


test<-as.data.frame(Cars$foreign)


N2<-Cars %>% 
  summarise(length(rep78))
N2


Cars%>%
  group_by(foreign)%>% 
  summarise(length(rep78),
            avg = mean(weight),
            local.avg2 = (foreign)^2*(avg*length(rep78)/(74)),
            sd(weight),
            weight.max = max(weight),
            weight.min = min(weight))  



session_info()
