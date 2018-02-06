# install necessary packages
PACKAGES<-c("broom", 
            "brew", 
            "dplyr", 
            "OECD", 
            "tempdisagg", 
            "tidyr",
            "reshape2",
            "ggplot2", 
            "haven",
            "qdap", 
            "tikzDevice", 
            "eurostat", 
            "lubridate", 
            "urca", 
            "stargazer", 
            "xtable", 
            "Hmisc", 
            "seasonal", 
            "vars", 
            "readxl", 
            "knitr")
inst<-match(PACKAGES, .packages(all=TRUE))
need<-which(is.na(inst))
if (length(need)>0) install.packages(PACKAGES[need])
# load required packages
lapply(PACKAGES, require, character.only=T)

# sink("Rpackage.bib")
# out <- sapply(names(sessionInfo()$otherPkgs), 
#               function(x) print(citation(x), style = "Bibtex"))
# toLatex(sessionInfo(), locale = FALSE)


# eurostat
clean_eurostat_cache()
# Get Eurostat data listing
toc <- get_eurostat_toc()
# Check the first items
kable(head(toc))
kable(head(search_eurostat("GDP and main components")))

dat <- get_eurostat("namq_10_gdp", time_format = "raw")
kable(head(dat))
str(dat)

kable(head(toc))
kable(head(search_eurostat("Non-financial transactions")))

dat2 <- get_eurostat("nasq_10_nf_tr", time_format = "raw")
kable(head(dat2))
str(dat2)


# Replacing codes with labels
dat_ext <- label_eurostat(dat)
kable(head(dat_ext))

# Code names
levels(dat$unit)
levels(dat$s_adj)
levels(dat$na_item)
levels(dat$geo)
levels(dat$time)
levels(dat$values)
str(dat_ext)


# get also labels for variable names
label_eurostat_vars(names(dat))


# Prefilter Unit of Measurement (nominal or real series) ------------------

dat_unit<-dat %>% 
  filter(unit == "CP_MEUR")


# B2G CEE excl. core ------------------------------------------------------

filter_CEE.dat<-dat_unit %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("EE","LT", "LV", "SK")) %>% # filter by country ## "SI" no data availablew
  # filter (unit == "CP_MEUR") %>% # filter: Current prices, million euro 
  filter (s_adj == "SWDA") %>% # filter: Seasonally adjusted and adjusted data by working days
  filter (na_item %in% c("B1G","D1","D2","D21","D3","D31")) %>% # filter by code item 
  spread(na_item, values) %>% # spread code item rows to columns 
  mutate(D29 = D2-D21) %>% # create new variable: D29
  mutate(D39 = D3-D31) %>%  # create new variable: D39
  mutate(net_taxes_on_production = D29-D39) %>% 
  arrange(geo, desc(time)) # sort time column


(filter_CEE.dat)


#### Note: 
# D21 and D2 for Slowakia are not available SWDA. The other inputs are not available SA. Individual analysis in the next section
####


B2G_CEE<-filter_CEE.dat %>% 
  # mutate(net_taxes_on_production = D29-D39) %>% # create new variable: net taxes on production = other taxes on production - other subsidies on production (tax burden)
  mutate(B2G = B1G-D1-D29+D39) %>% # create new variable: Gross operating surplus
  mutate(B2G_ratio_CEE = B2G/B1G*100) %>% 
  arrange(desc(time)) # sort time column

B2G_CEE
names(B2G_CEE)



### not an optimal data mangling solution. the gather/ melt did not work in the matrix. therefore individual geo calls and then cbind
# Estonia
B2G_ratio_EE<-B2G_CEE %>% 
  spread(geo, B2G_ratio_CEE) %>% 
  select(time, EE) %>% 
  arrange(desc(time)) %>% 
  filter(!is.na(EE))
B2G_ratio_EE

# Latvia
B2G_ratio_LV<-B2G_CEE %>% 
  spread(geo, B2G_ratio_CEE) %>% 
  select(time, LV) %>% 
  arrange(desc(time)) %>% 
  filter(!is.na(LV))

B2G_ratio_LV

# Lithuania
B2G_ratio_LT<-B2G_CEE %>% 
  spread(geo, B2G_ratio_CEE) %>% 
  select(time, LT) %>% 
  arrange(desc(time)) %>% 
  filter(!is.na(LT))
B2G_ratio_LT


library(plyr)
B2G_ratio_CEE<-plyr::join_all(list(B2G_ratio_EE,B2G_ratio_LT,B2G_ratio_LV), by='time', type='left')
B2G_ratio_CEE


B2G_ratio_CEE.ts<-ts(B2G_ratio_CEE, start = c(1995, 1), frequency = 4)# convert to time series object
B2G_ratio_CEE.ts<-B2G_ratio_CEE.ts[,-1] # remove row conversion for analysis
B2G_ratio_CEE.ts

# standard plot
plot(B2G_ratio_CEE.ts, facets = T)

library(ggplot2)
library(ggfortify)

autoplot(B2G_ratio_CEE.ts, facets = FALSE) +
  xlab("Year") + ylab("Percent") +
  # geom_line(aes(linetype=), # Line type depends on cond
  #        size = 1.5) +
  theme_bw()

# convert B2G_CEE matrix into a time series object
B2G_CEE.ts<-ts(B2G_CEE, start = c(1995, 1), frequency = 4)# convert to time series object
B2G_CEE.ts

# B2G SK -----------------------------------------------------

#### Note: 
# D21 and D2 for Slowakia are not available SWDA. Adjust the inputs individually
####

filter_SK_adj.dat<-dat_unit %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("SK")) %>% # filter by country
  filter (unit == "CP_MEUR") %>% # filter: Current prices, million euro 
  filter (s_adj %in% c("SA")) %>% # filter: Seasonally adjusted and adjusted data by working days
  filter (na_item %in% c("B1G","D1","D3","D31", "D21")) %>% # filter by code item 
  spread(na_item, values) %>% # spread code item rows to columns 
  #mutate(D29 = D2-D21) %>% # create new variable: D29
  #mutate(D39 = D3-D31) %>%  # create new variable: D39
  # mutate(net taxes on production = D29-D39) %>% 
  arrange(desc(time)) # sort time column

(filter_SK_adj.dat)

filter_SK_NSA_D2.dat<-dat_unit %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("SK")) %>% # filter by country
  filter (unit == "CP_MEUR") %>% # filter: Current prices, million euro 
  filter (s_adj %in% c("SA")) %>% # filter: Seasonally adjusted and adjusted data by working days
  filter (na_item %in% c("D2","D21")) %>% # filter by code item 
  spread(na_item, values) %>% # spread code item rows to columns 
  #mutate(D29 = D2-D21) %>% # create new variable: D29
  #mutate(D39 = D3-D31) %>%  # create new variable: D39
  # mutate(net taxes on production = D29-D39) %>% 
  arrange(desc(time)) # sort time column

(filter_SK_NSA_D2.dat)


####
# No D21 data from Eurostat. Obtain taxation and subsidiy data from OECD
# Only yearly frequency data for those inputs from OECD!
###

SK_anndat_taxes <- OECD::get_dataset("SNA_TABLE1",# quarterly national accounts
                  filter = "SVK.D21_D31+D2_D3.C", # unique SDMX identifier 
                  pre_formatted = F)

names(SK_anndat_taxes)

filter_SK_anndat_taxes<-SK_anndat_taxes %>% 
  spread(TRANSACT, obsValue) %>% 
  select(obsTime, D2_D3, D21_D31) %>% 
  mutate(D29_D39 = D2_D3- D21_D31) %>% 
  filter(obsTime >= 1995)
  
filter_SK_anndat_taxes


# convert to a ts object

SK_anndat_taxes.ts<-ts(filter_SK_anndat_taxes, start = c(1995, 1), frequency = 1)

plot(SK_anndat_taxes.ts[,4], typ="line")


# temporal disaggregation
td_SK_anndat_taxes.ts<-SK_anndat_taxes.ts[,4]

td_SK_quadat_D3.ts<-ts(filter_SK_adj.dat$D3, start = c(1995, 1), end = c(2014, 4), frequency = 4) #bring regressor to the same length

td_SK_quadat_D29_D39<-td(td_SK_anndat_taxes.ts ~ 0 + td_SK_quadat_D3.ts,
   conversion = "last",
   to = "quarterly",
   method = "chow-lin-minrss-quilis" # for movement preservation
  )

summary(td_SK_quadat_D29_D39)  # summary statistics
plot(td_SK_quadat_D29_D39)  # residual plot of regression
plot(predict(td_SK_quadat_D29_D39))

# interpolated (and extrapolated) high frequency series of the temporal disaggregation
td_SK_quadat_D29_D39.ts<-predict(td_SK_quadat_D29_D39)
td_SK_quadat_D29_D39.ts

# combine SK national accoount frame

class(td_SK_quadat_D29_D39.ts)
filter_SK_adj.dat.ts<-ts(filter_SK_adj.dat, start = c(1995, 1), frequency = 4) 
colnames(filter_SK_adj.dat.ts)
Complete_SK_adj.dat.ts<-cbind(filter_SK_adj.dat.ts,td_SK_quadat_D29_D39.ts )
Complete_SK_adj.dat.df<-as.data.frame(Complete_SK_adj.dat.ts)
names(Complete_SK_adj.dat.df)
Complete_SK_adj.dat.df<-Complete_SK_adj.dat.df %>% 
  #select(filter_SK_adj.dat.ts.B1G,filter_SK_adj.dat.ts.D1, td_SK_quadat_D29_D39.ts) #%>% 
  mutate(B2G = filter_SK_adj.dat.ts.B1G - filter_SK_adj.dat.ts.D1 - td_SK_quadat_D29_D39.ts) %>% 
  mutate(B2G_ratio_SK=B2G/filter_SK_adj.dat.ts.B1G*100)
Complete_SK_adj.dat.df

# B2G Austria & Germany -----------------------------------------------------

filter_core.dat<-dat_unit %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("AT")) %>% # filter by country
  filter (unit == "CP_MEUR") %>% # filter: Current prices, million euro 
  filter (s_adj %in% c("NSA")) %>% # filter: Seasonally adjusted and adjusted data by working days
  filter (na_item %in% c("B1G","D1","D2","D21","D3","D31")) %>% # filter by code item 
  spread(na_item, values) %>% # spread code item rows to columns 
  mutate(D29 = D2-D21) %>% # create new variable: D29
  mutate(D39 = D3-D31) %>%  # create new variable: D39
  # mutate(net taxes on production = D29-D39) %>% 
  arrange(desc(time)) # sort time column

(filter_core.dat)

#### Result 1: 
# there seems to be no data for "D21" and "D39" for Germany!
####

#### Result 2: 
# D2 is only available NSA for Austria and the 2015Q3 observations are incomplete
####

B2G_core<-filter_core.dat %>% 
  mutate(net_taxes_on_production = D29-D39) %>% # create new variable: net taxes on production = other taxes on production - other subsidies on production (tax burden)
  mutate(B2G = B1G-D1-D29+D39) %>%  # create new variable: Gross operating surplus
  arrange(desc(time)) # sort time column

B2G_core<-B2G_core %>% 
  filter(!is.na(B2G)) %>%  # filter out NA values
  mutate(B2G_ratio_core = B2G/B1G*100) # create new variable: B2G ration = B2G/B1G * 100

net_taxes_on_production.ts<-ts(B2G_core$net_taxes_on_production, start = c(1995, 1), frequency = 4)
plot(net_taxes_on_production.ts, type = "line")

B2G_core.ts<-ts(B2G_core, start = c(1995, 1), frequency = 4)# convert to time series object

## Seasonally Adjust the Austrian Data B2G series
dim(B2G_core.ts)
B2G_ratio_core.ts<-B2G_core.ts[,15]

# write(Sys.setenv(X13_PATH = "/Users/nicolasreigl/Downloads/x13assrc_V1.1_B19"),file = "~/.Rprofile", append = TRUE)
 # write('Sys.setenv(X13_PATH = "/Users/nicolasreigl/Downloads/x13assrc_V1.1_B19")', 
   #   file = "~/.Rprofile", append = TRUE)

checkX13()

SA_B2G_ratio_core.ts <- seas(B2G_ratio_core.ts)
summary(SA_B2G_ratio_core.ts)
# out(SA_B2G_ratio_core.ts)
static(SA_B2G_ratio_core.ts)

plot(SA_B2G_ratio_core.ts)
plot(resid(SA_B2G_ratio_core.ts), main = "Residuals")
qqnorm(resid(SA_B2G_ratio_core.ts), main = "Residuals compared to Normal")
pacf(resid(SA_B2G_ratio_core.ts), "Partial ACF of residuals")

SA_Dat_B2G_ratio_core.ts<-SA_B2G_ratio_core.ts$data[,1] # extract the seasonally adjusted series out of the seas object
SA_Dat_B2G_ratio_core.ts

# combine national account framwork ratios: CEE & AT ---------------------

class(B2G_ratio_CEE.ts)
class(SA_Dat_B2G_ratio_core.ts)


B2G_ratio_natacc.ts<-cbind(B2G_ratio_CEE.ts, SA_Dat_B2G_ratio_core.ts) # B2G ratio from the national accounts framework for all countries under anaylsis
colnames(B2G_ratio_natacc.ts)<-c("EE", "LT", "LV", "AT")
B2G_ratio_natacc.ts

autoplot(B2G_ratio_natacc.ts, facets = FALSE) +
  xlab("Year") + ylab("Percent") +
  # geom_line(aes(linetype=), # Line type depends on cond
  #        size = 1.5) +
  theme_bw()

# 






# Firms profit: Gross profit share of non-financial corporations --------

# Get Eurostat data listing
clean_eurostat_cache()
toc <- get_eurostat_toc()
# Check the first items
kable(head(toc))
kable(head(search_eurostat("Quarterly sector accounts")))

dat_micro <- get_eurostat("nasq_10_ki", time_format = "raw")
kable(head(dat_micro))
str(dat_micro)

# Replacing codes with labels
dat_micro_ext <- label_eurostat(dat_micro)
kable(head(dat_micro_ext))

# Code names
levels(dat_micro$unit)
levels(dat_micro$s_adj)
levels(dat_micro$na_item)
levels(dat_micro$geo)
levels(dat_micro$time)
levels(dat_micro$values)
levels(dat_micro$sector)


# get also labels for variable names
label_eurostat_vars(names(dat_micro_ext))

# Selecting and modifying data

filter.dat_micro<-dat_micro %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("EE","LT", "LV", "SK","AT")) %>% # filter by country
  filter (unit == "PC") %>% # filter: percent
  filter (s_adj == "NSA") %>% # filter: only seasonally adjusted
  filter (na_item %in% c("B2G_B3G_RAT_S11")) %>% # filter by code item: Gross profit share of non-financial corporations (B2G_B3G/B1G*100) 
  spread(na_item, values) %>% # spread code item rows to columns 
  # mutate(D29 = D2-D21) %>% # create new variable: D29
  # mutate(D39 = D3-D31) %>%  # create new variable: D39
  arrange(desc(time)) # sort time column
filter.dat_micro

## Analysis shows that there is no data for "LT", "LV", "SK" 

# create subsets for available countriess
filter.EE_micro<-filter.dat_micro %>% 
  filter(geo == c("EE")) %>% 
  select(B2G_B3G_RAT_S11)

filter.AT_micro<-filter.dat_micro %>% 
  filter(geo == c("AT")) %>% 
  select(B2G_B3G_RAT_S11)

GPS_AT.ts<-ts(data = filter.AT_micro, start = c(1999,1), frequency = 4) # gross profit share time series Austria
GPS_EE.ts<-ts(data = filter.EE_micro, start = c(1999,1), frequency = 4) # gross profit share time series Estonias


SA_GPS_AT.ts <- seas(GPS_AT.ts) # seasonal adjustment of the time series
SA_GPS_EE.ts <- seas(GPS_EE.ts) # seasonal adjustment of the time series


# extract seas. adjusted series
SA_Dat_GPS_AT.ts<-ts(data=SA_GPS_AT.ts$data[,1], start = c(1999,1), frequency = 4)
SA_Dat_GPS_EE.ts<-ts(data=SA_GPS_EE.ts$data[,1], start = c(1999,1), frequency = 4)
  


# Plot seas. adjustment gross profit share of non financial coporations

B2G_ratio_firmprof.ts<-cbind(SA_Dat_GPS_AT.ts,SA_Dat_GPS_EE.ts) # B2G ratio from the national accounts framework for all countries under anaylsis
colnames(B2G_ratio_firmprof.ts)<-c("AT","EE")
B2G_ratio_firmprof.ts

autoplot(B2G_ratio_firmprof.ts, facets = FALSE) +
  xlab("Year") + ylab("Percent") +
  # geom_line(aes(linetype=), # Line type depends on cond
  #        size = 1.5) +
  theme_bw()


# Merge National accounts method with firm's profits method ---------------
B2G_ratio_firmprof.ts
B2G_ratio_natacc.ts

Comp_B2G.ts<-cbind(B2G_ratio_firmprof.ts,
                   B2G_ratio_natacc.ts)

Comp_B2G.ts
colnames(Comp_B2G.ts) <- c("Gross profit share AT", "Gross profit share EE",
                           "B2G ratio EE",
                           "B2G ratio LT",
                           "B2G ratio LV",
                           "B2G ratio AT")

  autoplot(Comp_B2G.ts, facets = FALSE) +
           xlab("Year") + ylab("Percent") #+ theme_bw()
  
# combine SK result with CEE results
  B2G_ratio_SK.ts<-ts(Complete_SK_adj.dat.df$B2G_ratio_SK, start =c(1995,1), frequency = 4)
  B2G_ratio_SK.ts
  Comp_B2G_incSK.ts<-cbind(Comp_B2G.ts, B2G_ratio_SK.ts)
  Comp_B2G_incSK.ts 


    
  
# melt for different graph
library(reshape)

melt_Comp_B2G.ts<-melt(data.frame(time = as.numeric(time(Comp_B2G.ts)), Comp_B2G.ts), id.vars="time")

melt_Comp_B2G_incSK.ts<-melt(data.frame(time = as.numeric(time(Comp_B2G_incSK.ts)), Comp_B2G.ts), id.vars="time")



# Graphics ----------------------------------------------------------------
melt_Comp_B2G_incSK.ts


# All countries - all variables. Facets = F
Comp_B2G_incSK_out1<-ggplot(melt_Comp_B2G_incSK.ts,aes(time, value, # specify xy axis
                                           color = variable, # specify that the color may depend on the variable
                                           linetype = variable, # specify that the the line type may depend on the variable
                                           size=variable)) + # specify that the line size may depend on the variable
  geom_line() + # include geometric object
  scale_linetype_manual(values = c("twodash", "twodash", "solid","solid", "solid","solid","solid")) + # set line type individually
  scale_size_manual(values=c(1.0, 1.0, 0.8, 0.8, 0.8, 0.8,  0.8))+ # set line size individually
  labs(title = "Corporate margins in quarterly levels") + ylab("Percent")
Comp_B2G_incSK_out1


####
# Note: SK series looks seems to be countercyclical over spans of the series. 
# This might be related to the diaggregation process, the dispersion of the data (the imputs for computing the ration come from different databases)
# or country specific reasons. The SK series is of little explanatory power at this state. 
# Therefore removed from further analysis
###


# Countries ex SK - all variables. Facets = F
Comp_B2G_out1<-ggplot(melt_Comp_B2G.ts,aes(time, value, # specify xy axis
                                           color = variable, # specify that the color may depend on the variable
                                           linetype = variable, # specify that the the line type may depend on the variable
                                           size=variable)) + # specify that the line size may depend on the variable
                      geom_line() + # include geometric object
                      scale_linetype_manual(values = c("twodash", "twodash", "solid","solid", "solid","solid","solid")) + # set line type individually
                      scale_size_manual(values=c(1.0, 1.0, 0.8, 0.8, 0.8, 0.8,  0.8))+ # set line size individually
                      labs(title = "Corporate margins") + ylab("Percent")
Comp_B2G_out1


# All countries - all variables. Facets = T
Comp_B2G_out2<-ggplot(melt_Comp_B2G.ts,aes(time, value, # specify xy axis
                                           color = variable, # specify that the color may depend on the variable
                                           linetype = variable, # specify that the the line type may depend on the variable
                                           size=variable)) + # specify that the line size may depend on the variable
                      geom_line() + # include geometric object
                      scale_linetype_manual(values = c("twodash", "twodash", "solid","solid","solid","solid")) + # set line type individually
                      scale_size_manual(values=c(1.0, 1.0, 0.8, 0.8, 0.8, 0.8 ))+ # set line size individually
                      labs(title = "Corporate margins") + ylab("Percent")+
                      facet_grid(variable ~ .)
(Comp_B2G_out2)

# Estonia - all variables. Facets = T
meltMo2_Comp_B2G.ts<-melt_Comp_B2G.ts %>%
  filter(variable %in% c("B2G.ratio.EE","Gross.profit.share.EE"))# modify object to exclude certain columns

Comp_B2G_out4<-ggplot(meltMo2_Comp_B2G.ts,aes(time, value, # specify xy axis
                                              color = variable, # specify that the color may depend on the variable
                                              linetype = variable, # specify that the the line type may depend on the variable
                                              size=variable)) + # specify that the line size may depend on the variable
                      geom_line() + # include geometric object
                      scale_linetype_manual(values = c("twodash","solid")) + # set line type individually
                      scale_size_manual(values=c(1.0,  0.8))+ # set line size individually
                      labs(title = "Corporate margins Estonia") + ylab("Percent")
Comp_B2G_out4

# Austria Estonia - all variables. Facets = T
meltMo3_Comp_B2G.ts<-melt_Comp_B2G.ts %>%
  filter(variable %in% c("B2G.ratio.EE","Gross.profit.share.EE","B2G.ratio.AT","Gross.profit.share.AT"))# modify object to exclude certain columns
Comp_B2G_out5<-ggplot(meltMo3_Comp_B2G.ts,aes(time, value, # specify xy axis
                                              color = variable, # specify that the color may depend on the variable
                                              linetype = variable, # specify that the the line type may depend on the variable
                                              size=variable)) + # specify that the line size may depend on the variable
                      geom_line() + # include geometric object
                      scale_linetype_manual(values = c("twodash","twodash","solid","solid")) + # set line type individually
                      scale_size_manual(values=c(1.0, 1.0,  0.8, 0.8))+ # set line size individually
                      labs(title = "Corporate margins Austria Estonia") + ylab("Percent")
Comp_B2G_out5

# Graphics: Margins and real gdp growth -----------------------------------
B2G_dat_EE<-B2G_CEE %>%
  filter(geo == "EE")
B2G_dat_EE


B2G_yoy_EE_t1<-B2G_dat_EE %>%
  mutate(D1_ratio = (D1/B1G)*100) %>% # D1 ratio
  mutate(net_tax_ratio = (net_taxes_on_production/B1G)*100) %>% # yoy growth
  mutate(yoy_B1G = ((B1G-(lag(B1G, n=4)))/(lag(B1G, n=4)))*100) %>% # yoy growth
  mutate(yoy_D1_ratio = ((D1_ratio-(lag(D1_ratio, n=4)))/(lag(D1_ratio, n=4)))*100) %>% # yoy growth
  mutate(yoy_net_taxes_ratio = ((net_tax_ratio-(lag(net_tax_ratio, n=4)))/(lag(net_tax_ratio, n=4)))*100) %>%  # yoy growth
  mutate(yoy_B2G_ratio = ((B2G_ratio_CEE-(lag(B2G_ratio_CEE, n=4)))/(lag(B2G_ratio_CEE, n=4)))*100)


B2G_yoy_EE_t1
View(B2G_yoy_EE_t1)
dim(B2G_yoy_EE_t1)
plot(B2G_yoy_EE_t1$yoy_B2G_ratio, type = "line")
plot(B2G_yoy_EE_t1$yoy_D1_ratio, type = "line")
plot(B2G_yoy_EE_t1$yoy_B1G, type = "line")
plot(B2G_yoy_EE_t1$yoy_net_taxes_ratio, type = "line")
plot(B2G_yoy_EE_t1$yoy_net_taxes_ratio, type = "line")

####################################################################################################################
# All results obtained up to this point are nominal. 
# The input variables (D1, D2, D3, D21, D39) are not available at  real terms and/ or combined seasonal adjustments
# Deflate them manually
####################################################################################################################

# input data frame
B2G_CEE
dim(B2G_CEE)

######################### try only with estonia
head(dat$unit)
B1G_deflator<-dat %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("EE")) %>% # filter by country ## "SI" no data availablew
  filter (unit %in% c("CP_MEUR","CLV10_MEUR")) %>% # filter: Current prices, million euro 
  filter (s_adj == "SWDA") %>% # filter: Seasonally adjusted and adjusted data by working days
  filter (na_item %in% c("B1G"))%>%
  spread(na_item, values) %>%
  spread(unit, B1G) %>%
  mutate(B1G_deflator = CP_MEUR/CLV10_MEUR) %>% 
  arrange(geo, desc(time)) # sort time column

View(B1G_deflator)
dim(B1G_deflator)
B1G_deflator

# next step: cbind 
(filter_CEE.dat)

test_filter_EE<-filter_CEE.dat %>% 
  filter (geo %in% c("EE"))
test_filter_EE
# with the
(B1G_deflator)

inter_B2G_CEE<-cbind(test_filter_EE[,c(1:10)], B1G_deflator$B1G_deflator) # only retain the necessary input columns from B2G_CEE. easier for later mutate_each or lapply operations

inter_B2G_CEE
names(inter_B2G_CEE)

real_B2G_CEE<-inter_B2G_CEE %>% 
  mutate_each(funs(./B1G_deflator$B1G_deflator),-c(unit:time))  %>% # replaces only the current values of the numeric columns. 
  mutate(real_D29 = D2-D21) %>% # create new variable: D29
  mutate(real_D39 = D3-D31) %>%  # create new variable: D39
  mutate(real_net_taxes_on_production = real_D29-real_D39) %>% 
  mutate(real_B2G = B1G-D1-real_D29+real_D39) %>% 
  mutate(control1 = B1G-real_B2G-D1-real_net_taxes_on_production)
real_B2G_CEE

min(real_B2G_CEE$control1) # check if change in shares add up to approx. 0
max(real_B2G_CEE$control1)


############################################################################
# transform immediatly
############################################################################

names(real_B2G_CEE)

comp_B2G_EE<-real_B2G_CEE %>% 
  mutate(yoy_B1G = (B1G-lag(B1G, n=4))/lag(B1G, n=4)*100) %>% 
  mutate(yoy_D1 = (D1-lag(D1, n=4))/lag(B1G, n=4)*100) %>% 
  mutate(yoy_net_taxes_on_production = (real_net_taxes_on_production-lag(real_net_taxes_on_production, n=4))/lag(B1G, n=4)*100) %>% 
  mutate(yoy_B2G = (real_B2G-lag(real_B2G, n=4))/lag(B1G, n=4)*100) %>% 
  mutate(checksum1=((yoy_D1+ yoy_B2G+yoy_net_taxes_on_production))) %>% 
  mutate(control2= (yoy_B1G-checksum1))

comp_B2G_EE<-na.omit(comp_B2G_EE)

head(real_B2G_CEE)
real_B2G_EE<-as.data.frame(real_B2G_CEE$real_B2G)

write.table(real_B2G_EE, "real_B2G_EE.csv", sep = ";")


min(comp_B2G_EE$control2)
max(comp_B2G_EE$control2)

min(comp_B2G_EE$yoy_B1G)
max(comp_B2G_EE$yoy_B1G)

min(comp_B2G_EE$yoy_B2G)
max(comp_B2G_EE$yoy_B2G)

min(comp_B2G_EE$yoy_D1)
max(comp_B2G_EE$yoy_D1)


min(comp_B2G_EE$yoy_net_taxes_on_production)
max(comp_B2G_EE$yoy_net_taxes_on_production)


# Graph real variables ----------------------------------------------------

# extract and melt columns for the specific graphs
# Margins and real GDP growth

View((comp_B2G_EE))
MargRealGDPgrowth<-comp_B2G_EE %>% 
  select(time, yoy_B1G, yoy_B2G)

MargRealGDPgrowth.ts<-ts(MargRealGDPgrowth, start =c(1996,1), frequency = 4)
MargRealGDPgrowth.ts
MargRealGDPgrowth.ts<-MargRealGDPgrowth.ts[,-1]

MargRealGDPgrowth.ts

# melt
melt_MargRealGDPgrowth.ts<-melt(data.frame(time = as.numeric(time(MargRealGDPgrowth.ts)), MargRealGDPgrowth.ts), id.vars="time")
str(melt_MargRealGDPgrowth.ts)
melt_MargRealGDPgrowth.ts


# easier to change the variable name outside ggplot
levels(melt_MargRealGDPgrowth.ts$variable)[levels(melt_MargRealGDPgrowth.ts$variable)=="yoy_B1G"] <- "Real gross value added"
levels(melt_MargRealGDPgrowth.ts$variable)[levels(melt_MargRealGDPgrowth.ts$variable)=="yoy_B2G"] <- "Real gross operating surplus"

# Plot 1
plot_MargRealGDPgrowth<-ggplot(melt_MargRealGDPgrowth.ts,aes(time, value, # specify xy axis
                                           color = variable, # specify that the color may depend on the variable
                                           linetype = variable, # specify that the the line type may depend on the variable
                                           size=variable))  + # specify that the line size may depend on the variable
  geom_line() + # include geometric object
  scale_linetype_manual(values = c("solid", "twodash")) + # set line type individually
  scale_size_manual(values=c(1.0, 0.8))+
  scale_color_manual(values=c("red", "darkblue"))+
  scale_x_continuous(breaks = seq(1996, 2015, by = 2))+
  scale_y_continuous(breaks = seq(-25, 25, by = 5), # define axis ticks behaviour
                     minor_breaks = seq(-25, 25, 10))+ #define axis gridline behaviour
# #   
#    scale_color_manual(values=c("red", "darkblue"),
#                      breaks=c("yoy_B1G", "yoy_B2G"),
#                     labels=c("Real gross Value added", "Real gross operating surplus "))+
  xlab("") + 
  ylab("") + 
  
  theme(legend.title=element_blank())+
  
  ggtitle("Margins and Real GDP Growth\n(year-on-year, in %)")
  #labs(title = "Margins and Real GDP Growth\year-on-year, in %") +
# scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1))
plot_MargRealGDPgrowth

# unmelt for exporting using dcast

dcast_MargRealGDPgrowth.ts<-dcast(melt_MargRealGDPgrowth.ts, time ~ variable)



# COMPOSITION OF THE MOVEMENT IN REAL GDP
# contribution to the year-on-year change, percentage points
## extract variables for graph
# 
names(comp_B2G_EE)
filter_comp_B2G_EE<-comp_B2G_EE %>% 
  select(yoy_B1G,yoy_D1,yoy_net_taxes_on_production,yoy_B2G)

# create two different frames for the line and for the bar chart
subfilter1_comp_B2G_EE<-filter_comp_B2G_EE %>% 
  select(yoy_B1G)

subfilter2_comp_B2G_EE<-filter_comp_B2G_EE %>% 
  select(yoy_D1,yoy_net_taxes_on_production,yoy_B2G)



# create subset of share real B2G EE.ts 
graph1_real_B2G_EE.ts<-ts(subfilter1_comp_B2G_EE, start = c(1996,1),frequency = 4)
graph1_real_B2G_EE.ts # b1g
graph2_real_inputs_EE.ts<-ts(subfilter2_comp_B2G_EE, start = c(1996,1),frequency = 4)
graph2_real_inputs_EE.ts # other inputs



# melt objects
melt_graph1_real_input_EE.ts<-melt(data.frame(time = as.numeric(time(graph1_real_B2G_EE.ts)), graph1_real_B2G_EE.ts), id.vars="time")
melt_graph2_real_inputs_EE.ts<-melt(data.frame(time = as.numeric(time(graph2_real_inputs_EE.ts)), graph2_real_inputs_EE.ts), id.vars="time")
melt_graph2_real_inputs_EE.ts
#  put the two positive and negative data in separate layers
dat1 <- subset(melt_graph2_real_inputs_EE.ts,value >= 0)
dat2 <- subset(melt_graph2_real_inputs_EE.ts,value < 0)
dat1
dat2



# plot


p1 <- ggplot() +
    # add bar layer positive values 
  geom_bar(data=dat1, mapping=aes(x=time, y=value,fill=variable), 
           width = .1,stat='identity') +
    #add bar layer negative values
    geom_bar(data=dat2, mapping=aes(x=time, y=value,fill=variable),
           width = .1,stat='identity') +
  #add bar layer positive values
    geom_line(data=melt_graph1_real_input_EE.ts, mapping=aes(x=time, y=value, colour= "Real gross Value added \n(year-on-year change, in %)"), 
            ,size=1.05) +
    scale_colour_manual(values=c("red"))+ # this is important: without a manual colour scale the color = "Variable name" does not get plotted on the legend
   scale_x_continuous(breaks = seq(1996, 2015, by = 2))+
   scale_y_continuous(breaks = seq(-25, 25, by = 5), # define axis ticks behaviour
                   minor_breaks = seq(-25, 25, 10)) + #define axis gridline behaviour
  
  theme(legend.title=element_blank())+
  
    scale_fill_manual(values=c("#9370DB", "#9ACD32","#FFA500"),
                    breaks=c("yoy_D1", "yoy_net_taxes_on_production", "yoy_B2G"),
                    labels=c("Compensation of employees", "Net indirect taxes", "Gross operating surplus"))+
  xlab("") + 
  ylab("") + 
  
  
  ggtitle("COMPOSITION OF THE MOVEMENT IN REAL GDP
          \n(contribution to the year-on-year change, percentage points,
          unless otherwise stated)") 

p1

# unmelt for exporting with recombining (cbind)

dcast_graph2_real_inputs_EE.ts<-dcast(melt_graph2_real_inputs_EE.ts, time ~ variable)
dcast_graph1_real_inputs_EE.ts<-dcast(melt_graph1_real_input_EE.ts, time ~ variable)

# recombine
aggr_composition_real_inputs_EE.ts<-cbind(dcast_graph2_real_inputs_EE.ts,dcast_graph1_real_inputs_EE.ts)




#  Profit Margin Indicator Approach ---------------------------------------
# Get data ----------------------------------------------------------------

B1G_deflator
filter_EE_tax.ratios.dat<-filter_CEE.dat %>% 
  filter(geo == c("EE")) %>% 
  mutate(Tax.ratio_to_nominal_GDP = D21/B1G) %>% # indirect taxes!! 
  mutate(Subs.ratio_to_nominal_GDP = D31/B1G)  # indirect subsidies!!

filter_EE_tax.ratios.dat
GDP_deflator_at_basic_prices.df<-left_join(B1G_deflator, filter_EE_tax.ratios.dat, by ="time") %>% 
  mutate(GDP_deflator_at_basic_prices = B1G_deflator*(1-(Tax.ratio_to_nominal_GDP-Subs.ratio_to_nominal_GDP)))
  
View(GDP_deflator_at_basic_prices.df)
names(GDP_deflator_at_basic_prices.df)

# Unit labour costs
dat3 <- get_eurostat("namq_10_lp_ulc", time_format = "raw")
kable(head(dat3))
str(dat3)

filter_ULC_EE.dat<-dat3 %>% 
  filter(!is.na(values)) %>% # filter out NA values
  filter (geo %in% c("EE")) %>% # filter by country ## "SI" no data availablew
  # filter (unit == "CP_MEUR") %>% # filter: Current prices, million euro 
  filter (s_adj == "NSA")  %>% # filter: Seasonally adjusted and adjusted data by working days  
  filter(unit == "I10") %>% 
  filter(na_item  %in% c("NULC_PER", "RLPR_PER")) %>% 
  spread(na_item, values) %>% 
  arrange(desc(time))

filter_ULC_EE.dat

# check for seasonality
plot(filter_ULC_EE.dat$NULC_PER, type = "l")
plot(GDP_deflator_at_basic_prices.df$GDP_deflator_at_basic_prices, type = "l")
## seasonal and volatile pattern in the NULC

## Seasonally Adjust the Austrian Data B2G series

NULC_PER.df<-filter_ULC_EE.dat$NULC_PER
NULC_PER.ts<-ts(NULC_PER.df, start = c(1995,1), frequency = 4)

SA_NULC_PER.ts <- seas(NULC_PER.ts)
summary(SA_NULC_PER.ts)
# out(SA_B2G_ratio_core.ts)
static(SA_NULC_PER.ts)

plot(SA_NULC_PER.ts)
plot(resid(SA_NULC_PER.ts), main = "Residuals")
qqnorm(resid(SA_NULC_PER.ts), main = "Residuals compared to Normal")
pacf(resid(SA_NULC_PER.ts), "Partial ACF of residuals")

extr_SA_NULC_PER.ts<-SA_NULC_PER.ts$data[,1] # extract the seasonally adjusted series out of the seas object
plot(extr_SA_NULC_PER.ts)

# add to main dataframe
extr_SA_NULC_PER.df<-as.data.frame(extr_SA_NULC_PER.ts)
extr_SA_NULC_PER.df
colnames(extr_SA_NULC_PER.df) <- c("SA_NULC_PER")
SANulc_filter_ULC_EE.dat<-cbind(filter_ULC_EE.dat, extr_SA_NULC_PER.df)
SANulc_filter_ULC_EE.dat
GDP_deflator_at_basic_prices.df

# profit margin season. adju
profit_margin_indicator1.df<-left_join(GDP_deflator_at_basic_prices.df, SANulc_filter_ULC_EE.dat, by = "time") %>% 
  mutate(profit_margin_indicator = GDP_deflator_at_basic_prices/SA_NULC_PER )

profit_margin_indicator1.df
dim(profit_margin_indicator1.df)
test1<-ts(profit_margin_indicator1.df, start=c(1995,1), frequency = 4)

# write.table(profit_margin_indicator1.df, "profit_margin_indicator1.df.csv", sep = ",")


dim(profit_margin_indicator1.df)
plot(profit_margin_indicator1.df$profit_margin_indicator, type = "l")

# profit margin season. unadj. 
profit_margin_indicator2.df<-left_join(GDP_deflator_at_basic_prices.df, SANulc_filter_ULC_EE.dat, by = "time") %>% 
  mutate(profit_margin_indicator = GDP_deflator_at_basic_prices/NULC_PER )

dim(profit_margin_indicator2.df)
plot(profit_margin_indicator2.df$profit_margin_indicator, type = "l")

plot(profit_margin_indicator1.df$profit_margin_indicator, type = "l", col = "darkgreen")
par(new=T)
plot(profit_margin_indicator2.df$profit_margin_indicator, type = "l", col = "red")
profit_margin_indicator1.df$profit_margin_indicator

# Calculate YoY growth rate
# define YoY function that can be "piped"
YearOnYear<-function (x,periodsPerYear){ #  periodsPerYear is for example Quarterly Values 4
  if(NROW(x)<=periodsPerYear){
    stop("too few rows")
  }
  else{
    indexes<-1:(NROW(x)-periodsPerYear)
    return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])/x[indexes]*100))
  }
}


profit_margin_indicator1.df
yoy.profit_margin_indicator1.df<-profit_margin_indicator1.df %>% 
  select(profit_margin_indicator ) %>% 
  mutate_each(funs(YearOnYear(.,4)))

yoy.profit_margin_indicator1.df
# SA_profitmargin_indicator<-(profit_margin_indicator1.df[,28])
# write.csv(SA_profitmargin_indicator, "SA_profitmargin_indicator.csv")

yoy.profit_margin_indicator.ts<-ts(yoy.profit_margin_indicator1.df, start = c(1995,1), frequency = 4)
plot(yoy.profit_margin_indicator.ts)
yoy.profit_margin_indicator.ts

grossOperatingSurplus_vs_profitMarginIndicator.ts<-na.omit(cbind(MargRealGDPgrowth.ts, yoy.profit_margin_indicator.ts))

# melt
melt_grossOperatingSurplus_vs_profitMarginIndicator.ts<-melt(data.frame(time = as.numeric(time(grossOperatingSurplus_vs_profitMarginIndicator.ts))
                                                                        , grossOperatingSurplus_vs_profitMarginIndicator.ts), id.vars="time")
str(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts)
melt_grossOperatingSurplus_vs_profitMarginIndicator.ts


# easier to change the variable name outside ggplot
levels(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts$variable)[levels(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts$variable)=="MargRealGDPgrowth.ts.yoy_B1G"] <- "Real gross value added"
levels(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts$variable)[levels(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts$variable)=="MargRealGDPgrowth.ts.yoy_B2G"] <- "Real gross operating surplus"
levels(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts$variable)[levels(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts$variable)=="yoy.profit_margin_indicator.ts"] <- "Profit margin indicator"
melt_grossOperatingSurplus_vs_profitMarginIndicator.ts

# Plot 2

grossOperatingSurplus_vs_profitMarginIndicator<-ggplot(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts,aes(time, value, # specify xy axis
                                                             color = variable, # specify that the color may depend on the variable
                                                             linetype = variable, # specify that the the line type may depend on the variable
                                                             size=variable))  + # specify that the line size may depend on the variable
  geom_line() + # include geometric object
  scale_linetype_manual(values = c("solid", "twodash", "dashed")) + # set line type individually
  scale_size_manual(values=c(1.0, 0.8, 0.7))+
  scale_color_manual(values=c("red", "darkblue", "darkgreen"))+
  scale_x_continuous(breaks = seq(1996, 2015, by = 2))+
  scale_y_continuous(breaks = seq(-25, 25, by = 5), # define axis ticks behaviour
                     minor_breaks = seq(-25, 25, 10))+ #define axis gridline behaviour
  # #   
  #    scale_color_manual(values=c("red", "darkblue"),
  #                      breaks=c("yoy_B1G", "yoy_B2G"),
  #                     labels=c("Real gross Value added", "Real gross operating surplus "))+
  xlab("") + 
  ylab("") + 
  
  theme(legend.title=element_blank())+
  
  ggtitle("Gross operating surplus vs. Profit margin indicator \n (year-on-year, in %)")

grossOperatingSurplus_vs_profitMarginIndicator

# unmelt for exporting
dcast_grossOperatingSurplus_vs_profitMarginIndicator.ts<-dcast(melt_grossOperatingSurplus_vs_profitMarginIndicator.ts, time ~ variable)
dcast_grossOperatingSurplus_vs_profitMarginIndicator.ts

write.table(dcast_grossOperatingSurplus_vs_profitMarginIndicator.ts, "dcast_grossOperatingSurplus_vs_profitMarginIndicator.ts.csv", sep = ",")





# # Firm level statistics ---------------------------------------------------
# 
# # sectoral markup from stata; based on Jaanikas anaylsis. Adopted with any changes --------
# sectoral.markup_EE<- read.csv("~/projects/enterprise profitability/jaanika firm level approach/sectoral markup_trimmed.csv")
# 
# head(sectoral.markup_EE)
# max(sectoral.markup_EE$pcm_tradable2)
# min(sectoral.markup_EE$pcm_tradable2)
# # 
# max(sectoral.markup_EE$pcm_ntradable2)
# min(sectoral.markup_EE$pcm_ntradable2)
# 
# sectoral.markup_EE<-sectoral.markup_EE %>% 
#   select(pcm_tradable2, pcm_ntradable2)
# 
# sectoral.markup_EE.ts<-ts(sectoral.markup_EE, start = c(1995,1),frequency = 1)
# melt_sectoral.markup_EE.ts<-melt(data.frame(time = as.numeric(time(sectoral.markup_EE.ts))
#                                                                         , sectoral.markup_EE.ts), id.vars="time")
# str(sectoral.markup_EE.ts)
# melt_sectoral.markup_EE.ts
# 
# 
# # easier to change the variable name outside ggplot
# levels(melt_sectoral.markup_EE.ts$variable)[levels(melt_sectoral.markup_EE.ts$variable)=="pcm_tradable2"] <- "Tradable sectors"
# levels(melt_sectoral.markup_EE.ts$variable)[levels(melt_sectoral.markup_EE.ts$variable)=="pcm_ntradable2"] <- "Non-tradable sectors"
# 
# plot_sectoral.markup_EE.ts<-ggplot(melt_sectoral.markup_EE.ts,aes(time, value, # specify xy axis
#                                                                   color = variable, # specify that the color may depend on the variable
#                                                                   linetype = variable, # specify that the the line type may depend on the variable
#                                                                   size=variable))  + # specify that the line size may depend on the variable
#   geom_line() + # include geometric object
#   scale_linetype_manual(values = c("solid", "twodash")) + # set line type individually
#   scale_size_manual(values=c(1.0, 0.8))+
#   scale_color_manual(values=c("darkred", "darkgreen"))+
#   scale_x_continuous(breaks = seq(1995, 2014, by = 2))+
#   scale_y_continuous(breaks = seq(-0.4, 0.15, by =.01), # define axis ticks behaviour
#                      minor_breaks = seq(-.01, .01, .01))+ #define axis gridline behaviour
#   xlab("") + 
#   ylab("") + 
#   
#   theme(legend.title=element_blank())+
#   
#   ggtitle("Price-cost margins in EE: tradables vs. non-tradables")
# 
# plot_sectoral.markup_EE.ts
# 
# # unmelt for exporting
# dcast_melt_sectoral.markup_EE.ts<-dcast(melt_sectoral.markup_EE.ts, time ~ variable)
# dcast_melt_sectoral.markup_EE.ts
# 
# # aggr. firm level data
# markup_EE<- read.csv("~/projects/enterprise profitability/jaanika firm level approach/markup_trimmed.csv")
# 
# tidy_markup_EE<-markup_EE %>% 
#   select(year, sector, pcm_w_w_trim) %>% 
#   group_by(year, sector, pcm_w_w_trim) %>% 
#   spread(year, pcm_w_w_trim)
# 
# mean_markup_EE<-as.data.frame(colMeans(tidy_markup_EE, na=T))
# View(mean_markup_EE)
# mean_markup_EE<-mean_markup_EE[-1,]
# mean_markup_EE.ts<-ts(mean_markup_EE, start =c(1995,1), frequency = 1)
# melt_mean_markup_EE.ts<-melt(data.frame(time = as.numeric(time(mean_markup_EE.ts))
#                                             , mean_markup_EE.ts), id.vars="time")
# mean_markup_EE.ts
# levels(melt_mean_markup_EE.ts$variable)[levels(melt_mean_markup_EE.ts$variable)=="mean_markup_EE.ts"] <-"Tradables and Non-Tradables"
# melt_mean_markup_EE.ts
# plot_agg.markup_EE.ts<-ggplot(melt_mean_markup_EE.ts,aes(time, value, # specify xy axis
#                                                                   color = variable, # specify that the color may depend on the variable
#                                                                   linetype = variable, # specify that the the line type may depend on the variable
#                                                                   size=variable))  + # specify that the line size may depend on the variable
#   geom_line() + # include geometric object
#   scale_linetype_manual(values = c("solid")) + # set line type individually
#   scale_size_manual(values=c(1.0))+
#   scale_color_manual(values=c("darkblue"))+
#   scale_x_continuous(breaks = seq(1995, 2014, by = 2))+
#   scale_y_continuous(breaks = seq(-0.4, 0.15, by =.01), # define axis ticks behaviour
#                      minor_breaks = seq(-.01, .01, .01))+ #define axis gridline behaviour
#   xlab("") + 
#   ylab("") + 
#   
#   theme(legend.title=element_blank())+
#   
#   ggtitle("Price-cost margins in EE")
# plot_agg.markup_EE.ts
# 
# 
# 
# # Estonian statistic: EMTAK -----------------------------------------------
# ## tidious data munging process
# 
# # 1. Caculate sectoral deflators
# NAA0045sm <- read.csv("~/projects/enterprise profitability/data/eesti statistic/NAA0045sm(1).csv", sep=";")
# View(NAA0045sm)
# 
# names(NAA0045sm)
# 
# f_NAA0045sm<-NAA0045sm %>% 
#   mutate(sec_deflator = Value.at.current.prices/Chain.linked.volume) # %>% 
# #   select(Year, Sector, sec_deflator) %>% 
# #   spread(Sector, sec_deflator)
# View(f_NAA0045sm)
# 
# NAA0043sm <- read.csv("~/projects/enterprise profitability/data/eesti statistic/NAA0043sm.csv", sep=";")
#    View(NAA0043sm)
#    head(NAA0043sm)
# 
#    
#    NAA0043sm$row <- 1:nrow(NAA0043sm) # add row number to have unique identifiers
# f_NAA0043sm<-NAA0043sm %>% 
#    filter(Sector != "Economic activities total") %>%  # exclude all activities
#    select(row, Year, Sector, component, NonFinancial.corporations) # %>% 
# 
# View(f_NAA0043sm)
# names(f_NAA0043sm)
# 
# dat_a<-right_join(f_NAA0043sm,f_NAA0045sm,  by =c("Year", "Sector")) %>% 
#   select (-Chain.linked.volume, -Value.at.current.prices) %>% 
#   mutate(real_components = NonFinancial.corporations/sec_deflator )
# View(dat_a)
# names(dat_a)
# 
# dat_b<-dat_a %>% 
#      select(-NonFinancial.corporations, -sec_deflator, -row) %>% 
#      spread(Sector, real_components) 
# View(dat_b)
# colnames(dat_b)
#  
# 
# drops <- c("Financial and insurance activities", "Professional, scientific and technical activities","Public administration and defence; compulsory social security")
# dat_c<-dat_b[,!(names(dat_b) %in% drops)] # remove columns with either missing or zero value data
# dat_d<-na.omit(dat_c) # remove the remaining NA rows
# 
# dat_e<-dat_d %>% 
#   filter (component != "..operating surplus and mixed income") %>% # to avoid rounding mistakes filter out the operating surplus
#   filter (component != "Output") %>% # and other not necessary components
#   filter (component != "Intermediate consumption")
# 
# 
# # construct sectoral disaggregates: tradables and non-tradables
# tradables_ex <- c("Agriculture, forestry and fishing" , "Manufacturing" ,"Mining and quarrying")
# tradables_inc <- c("Year","component", "Agriculture, forestry and fishing" , "Manufacturing" ,"Mining and quarrying")
# tradables_disagg<-dat_e[,(names(dat_e) %in% tradables_inc)]
# non_tradables_disagg<-dat_e[,!(names(dat_e) %in% tradables_ex)] # remove columns with either missing or zero value data
# View(non_tradables)
# View(tradables)
# 
# colnames((non_tradables_disagg))
# dim(non_tradables_disagg)
# View(non_tradables_disagg)
# 
# # manipulate  non-tradables
# non_tradables<-non_tradables_disagg %>% 
#   mutate(sum = rowSums(.[3:15]))
# View(non_tradables)
# 
# 
# spread_non_tradables<-non_tradables %>% 
#   select(Year, component, sum ) %>% 
#   spread(Year, sum)
# 
# View(spread_non_tradables)
# 
# names(spread_non_tradables)
# t_spread_non_tradables<-setNames(data.frame(t(spread_non_tradables[,-1])), spread_non_tradables[,1])
# t_spread_non_tradables
# 
# View(t_spread_non_tradables)
# t1_spread_non_tradables<-t_spread_non_tradables %>% 
#   mutate(operating_surplus_and_mixed_income = `Value added`-`..consumption of fixed capital`
#          -`..employers' social contribution`-`..net taxes on production`-`..wages and salaries`)
# 
# View(t1_spread_non_tradables)
# write.table(t1_spread_non_tradables, "t1_spread_non_tradables.csv",sep =";")
# 
# 
# names(t1_spread_non_tradables)
# 
# yoy.t1_spread_non_tradables<-t1_spread_non_tradables %>% 
#   mutate(yoy_B1G = ( `Value added`-lag( `Value added`, n=1))/lag( `Value added`, n=1)*100) %>% 
#   mutate(yoy_D1 = (`..wages and salaries`-lag(`..wages and salaries`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_consfixcap = (`..consumption of fixed capital`-lag(`..consumption of fixed capital`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_socialcont = (`..employers' social contribution`-lag(`..employers' social contribution`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_net_taxes_on_production = (`..net taxes on production`-lag(`..net taxes on production`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_B2GB3G = (operating_surplus_and_mixed_income-lag(operating_surplus_and_mixed_income, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(checksum1=((yoy_D1+ yoy_B2GB3G+yoy_net_taxes_on_production+yoy_socialcont+yoy_consfixcap))) %>% 
#   mutate(control2= (yoy_B1G-checksum1))
# 
# yoy.t1_spread_non_tradables<-na.omit(yoy.t1_spread_non_tradables)
# View(yoy.t1_spread_non_tradables)
# 
# # manipulate tradables
# tradables<-tradables_disagg %>% 
#   mutate(tradables = `Agriculture, forestry and fishing`+`Manufacturing`)
# View(tradables)
# 
# spread_tradables<-tradables %>% 
#   select(Year, component, tradables ) %>% 
#   spread(Year, tradables)
# 
# 
# View(spread_tradables)
# 
# names(spread_tradables)
# t_spread_tradables<-setNames(data.frame(t(spread_tradables[,-1])), spread_tradables[,1])
# t_spread_tradables
# 
# View(t_spread_tradables)
# t1_spread_tradables<-t_spread_tradables %>% 
#   mutate(operating_surplus_and_mixed_income = `Value added`-`..consumption of fixed capital`
#          -`..employers' social contribution`-`..net taxes on production`-`..wages and salaries`) # the ´after employers social contribution might create problems in different versions. Sanitize if necessary
# 
# View(t1_spread_tradables)
# 
# names(t1_spread_tradables)
# 
# View(real_B2G_CEE)
# 
# yoy.t1_spread_tradables<-t1_spread_tradables %>% 
#   mutate(yoy_B1G = ( `Value added`-lag( `Value added`, n=1))/lag( `Value added`, n=1)*100) %>% 
#   mutate(yoy_D1 = (`..wages and salaries`-lag(`..wages and salaries`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_consfixcap = (`..consumption of fixed capital`-lag(`..consumption of fixed capital`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_socialcont = (`..employers' social contribution`-lag(`..employers' social contribution`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_net_taxes_on_production = (`..net taxes on production`-lag(`..net taxes on production`, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(yoy_B2GB3G = (operating_surplus_and_mixed_income-lag(operating_surplus_and_mixed_income, n=1))/lag(`Value added`, n=1)*100) %>% 
#   mutate(checksum1=((yoy_D1+ yoy_B2GB3G+yoy_net_taxes_on_production+yoy_socialcont+yoy_consfixcap))) %>% 
#   mutate(control2= (yoy_B1G-checksum1))
# 
# yoy.t1_spread_tradables<-na.omit(yoy.t1_spread_tradables)
# View(yoy.t1_spread_tradables)
# 
# head(t1_spread_tradables)
# dim(t1_spread_tradables)
# View(t1_spread_tradables)
# write.table(t1_spread_tradables, "t1_spread_tradables.csv", sep =";")
# 
# 
# #############################################
# # End of sectoral Markup calculation section 
# #############################################
# 
# 
# # Graphics: Sectoral Markups ----------------------------------------------
# 
# # COMPOSITION OF THE MOVEMENT IN REAL GDP
# # contribution to the year-on-year change, percentage points
# ## extract variables for graph
# 
# 
# # 1) Graph tradables 
# names(yoy.t1_spread_tradables)
# filter_yoy.t1_spread_tradables<-yoy.t1_spread_tradables %>% 
#   select(yoy_B1G,yoy_D1,yoy_net_taxes_on_production,yoy_consfixcap,yoy_socialcont,yoy_B2GB3G)
# 
# # create two different frames for the line and for the bar chart
# subfilter1_yoy.t1_spread_tradables<-filter_yoy.t1_spread_tradables %>% 
#   select(yoy_B1G)
# 
# subfilter2_yoy.t1_spread_tradables<-filter_yoy.t1_spread_tradables %>% 
#   select(yoy_D1,yoy_net_taxes_on_production,yoy_consfixcap,yoy_socialcont,yoy_B2GB3G)
# 
# # create subset of share real B2G EE.ts 
# graph1_tradables.ts<-ts(subfilter1_yoy.t1_spread_tradables, start = c(1996,1),frequency = 1)
# graph1_tradables.ts # b1g
# graph2_tradables.ts<-ts(subfilter2_yoy.t1_spread_tradables, start = c(1996,1),frequency = 1)
# graph2_tradables.ts # other inputs
# 
# 
# # melt objects
# melt_graph1_tradables.ts<-melt(data.frame(time = as.numeric(time(graph1_tradables.ts)), graph1_tradables.ts), id.vars="time")
# melt_graph2_tradables.ts<-melt(data.frame(time = as.numeric(time(graph2_tradables.ts)), graph2_tradables.ts), id.vars="time")
# 
# #  put the two positive and negative data in separate layers
# dat1_tradables <- subset(melt_graph2_tradables.ts,value >= 0)
# dat2_tradables <- subset(melt_graph2_tradables.ts,value < 0)
# dat1_tradables
# dat2_tradables
# 
# 
# # plot
# p1_tradables <- ggplot() +
#   # add bar layer positive values 
#   geom_bar(data=dat1_tradables, mapping=aes(x=time, y=value,fill=variable), 
#            width = .8,stat='identity') +
#   #add bar layer negative values
#   geom_bar(data=dat2_tradables, mapping=aes(x=time, y=value,fill=variable),
#            width = .8,stat='identity') +
#   #add bar layer positive values
#   geom_line(data=melt_graph1_tradables.ts, mapping=aes(x=time, y=value, colour= "Real gross Value added \n(year-on-year change, in %)"), 
#             ,size=1.2) +
#   scale_colour_manual(values=c("red"))+ # this is important: without a manual colour scale the color = "Variable name" does not get plotted on the legend
#   scale_x_continuous(breaks = seq(1996, 2014, by = 2))+
#   scale_y_continuous(breaks = seq(-25, 25, by = 5), # define axis ticks behaviour
#                      minor_breaks = seq(-25, 25, 10)) + #define axis gridline behaviour
#   
#   theme(legend.title=element_blank())+
#   
#   scale_fill_manual(values=c("#9370DB", "#9ACD32","#ffeda0", "#2b8cbe","#FFA500" ),
#                     breaks=c("yoy_D1", "yoy_net_taxes_on_production","yoy_consfixcap","yoy_socialcont","yoy_B2GB3G"),
#                     labels=c("Wages and salaries",
#                              "Net taxes on production",
#                              "Consumption of fixed capital",
#                              "Employers' social contribution",
#                              "Operating surplus and mixed income"))+
#   xlab("") + 
#   ylab("") + 
#   
#   
#   ggtitle("COMPOSITION OF THE MOVEMENT IN REAL GDP - Tradable Sectors
#           \n(contribution to the year-on-year change, percentage points,
#           unless otherwise stated)") 
# 
# p1_tradables
# p1
# 
# # unmelt for exporting
# melt_graph1_tradables.ts
# melt_graph2_tradables.ts
# 
# dcast_graph1_tradables.ts<-dcast(melt_graph1_tradables.ts, time ~ variable)
# dcast_graph2_tradables.ts<-dcast(melt_graph2_tradables.ts, time ~ variable)
# comp_tradables_EE.ts<-cbind(dcast_melt_graph1_tradables.ts, dcast_melt_graph2_tradables.ts)
# 
# 
# # 2) Graph non-tradables 
# names(yoy.t1_spread_non_tradables)
# filter_yoy.t1_spread_non_tradables<-yoy.t1_spread_non_tradables %>% 
#   select(yoy_B1G,yoy_D1,yoy_net_taxes_on_production,yoy_consfixcap,yoy_socialcont,yoy_B2GB3G)
# 
# # create two different frames for the line and for the bar chart
# subfilter1_yoy.t1_spread_non_tradables<-filter_yoy.t1_spread_non_tradables %>% 
#   select(yoy_B1G)
# 
# subfilter2_yoy.t1_spread_non_tradables<-filter_yoy.t1_spread_non_tradables %>% 
#   select(yoy_D1,yoy_net_taxes_on_production,yoy_consfixcap,yoy_socialcont,yoy_B2GB3G)
# 
# # create subset of share real B2G EE.ts 
# graph1_non_tradables.ts<-ts(subfilter1_yoy.t1_spread_non_tradables, start = c(1996,1),frequency = 1)
# graph1_non_tradables.ts # b1g
# graph2_non_tradables.ts<-ts(subfilter2_yoy.t1_spread_non_tradables, start = c(1996,1),frequency = 1)
# graph2_non_tradables.ts # other inputs
# 
# 
# # melt objects
# melt_graph1_non_tradables.ts<-melt(data.frame(time = as.numeric(time(graph1_non_tradables.ts)), graph1_non_tradables.ts), id.vars="time")
# melt_graph2_non_tradables.ts<-melt(data.frame(time = as.numeric(time(graph2_non_tradables.ts)), graph2_non_tradables.ts), id.vars="time")
# melt_graph2_real_inputs_EE.ts
# 
# #  put the two positive and negative data in separate layers
# dat1_non_tradables <- subset(melt_graph2_non_tradables.ts,value >= 0)
# dat2_non_tradables <- subset(melt_graph2_non_tradables.ts,value < 0)
# dat1_non_tradables
# dat2_non_tradables
# 
# 
# head(NAA0043sm)
# # plot
# p1_non_tradables <- ggplot() +
#   # add bar layer positive values 
#   geom_bar(data=dat1_non_tradables, mapping=aes(x=time, y=value,fill=variable), 
#            width = .8,stat='identity') +
#   #add bar layer negative values
#   geom_bar(data=dat2_non_tradables, mapping=aes(x=time, y=value,fill=variable),
#            width = .8,stat='identity') +
#   #add bar layer positive values
#   geom_line(data=melt_graph1_non_tradables.ts, mapping=aes(x=time, y=value, colour= "Real gross Value added \n(year-on-year change, in %)"), 
#             ,size=1.2) +
#   scale_colour_manual(values=c("red"))+ # this is important: without a manual colour scale the color = "Variable name" does not get plotted on the legend
#   scale_x_continuous(breaks = seq(1996, 2014, by = 2))+
#   scale_y_continuous(breaks = seq(-25, 25, by = 5), # define axis ticks behaviour
#                      minor_breaks = seq(-25, 25, 10)) + #define axis gridline behaviour
#   
#   theme(legend.title=element_blank())+
#   
#   scale_fill_manual(values=c("#9370DB", "#9ACD32","#ffeda0", "#2b8cbe","#FFA500" ),
#                     breaks=c("yoy_D1", "yoy_net_taxes_on_production","yoy_consfixcap","yoy_socialcont","yoy_B2GB3G"),
#                     labels=c("Wages and salaries",
#                              "Net taxes on production",
#                              "Consumption of fixed capital",
#                              "Employers' social contribution",
#                              "Operating surplus and mixed income"))+
#   xlab("") + 
#   ylab("") + 
#   
#   
#   ggtitle("COMPOSITION OF THE MOVEMENT IN REAL GDP - Non-tradable Sectors
#           \n(contribution to the year-on-year change, percentage points,
#           unless otherwise stated)") 
# 
# p1_non_tradables
# 
# 
# # unmelt for exporting
# melt_graph1_non_tradables.ts
# melt_graph2_non_tradables.ts
# 
# dcast_graph1_non_tradables.ts<-dcast(melt_graph1_non_tradables.ts, time ~ variable)
# dcast_graph2_non_tradables.ts<-dcast(melt_graph2_non_tradables.ts, time ~ variable)
# comp_non_tradables_EE.ts<-cbind(dcast_graph1_non_tradables.ts, dcast_graph2_non_tradables.ts)
# 
# 
# # 3) Plot tradable vs. non-tradable sector
# graph2_non_tradables_B2GB3G.ts<-graph2_non_tradables.ts[,5]
# graph2_tradables_B2GB3G.ts<-graph2_tradables.ts[,5]
# graph2_tradables_B2GB3G.ts
# graph2_non_tradables_B2GB3G.ts
# 
# B2GB3G_comp.ts<-cbind(graph2_tradables_B2GB3G.ts,
#       graph2_non_tradables_B2GB3G.ts)
# 
# melt_B2GB3G_comp.ts<-melt(data.frame(time = as.numeric(time(B2GB3G_comp.ts)), B2GB3G_comp.ts), id.vars="time")
# melt_B2GB3G_comp.ts
# 
# # easier to change the variable name outside ggplot
# levels(melt_B2GB3G_comp.ts$variable)[levels(melt_B2GB3G_comp.ts$variable)=="graph2_tradables_B2GB3G.ts"] <- "Operating surplus and mixed income Tradable Sectors"
# levels(melt_B2GB3G_comp.ts$variable)[levels(melt_B2GB3G_comp.ts$variable)=="graph2_non_tradables_B2GB3G.ts"] <- "Operating surplus and mixed income Non-tradable Sectors "
# melt_B2GB3G_comp.ts
# 
# 
# plot_B2GB3G_comp.ts<-ggplot(melt_B2GB3G_comp.ts,aes(time, value, # specify xy axis
#                                                                   color = variable, # specify that the color may depend on the variable
#                                                                   linetype = variable, # specify that the the line type may depend on the variable
#                                                                   size=variable))  + # specify that the line size may depend on the variable
#   geom_line() + # include geometric object
#   scale_linetype_manual(values = c("solid", "twodash")) + # set line type individually
#   scale_size_manual(values=c(1.0, 0.8))+
#   scale_color_manual(values=c("darkred", "darkgreen"))+
#   scale_x_continuous(breaks = seq(1996, 2014, by = 2))+
#   scale_y_continuous(breaks = seq(-13, 19, by =1), # define axis ticks behaviour
#                      minor_breaks = seq(-13, 19, 10))+ #define axis gridline behaviour
#   xlab("") + 
#   ylab("") + 
#   
#   theme(legend.title=element_blank())+
#   
#   ggtitle("Operating surplus and mixed income in EE: tradables vs. non-tradables \n (year-on-year, in %)")
# 
# plot_B2GB3G_comp.ts
# 
# # unmelt for exporting
# dcast_B2GB3G_comp.ts<-dcast(melt_B2GB3G_comp.ts, time ~ variable)
# dcast_B2GB3G_comp.ts
# 
# 
# dcast_MargRealGDPgrowth.ts
# aggr_composition_real_inputs_EE.ts
# dcast_grossOperatingSurplus_vs_profitMarginIndicator.ts
# dcast_melt_sectoral.markup_EE.ts
# mean_markup_EE.ts
# # comp_tradables_EE.ts<-as.data.frame(comp_tradables_EE.ts)
# comp_tradables_EE.ts
# comp_non_tradables_EE.ts
# dcast_B2GB3G_comp.ts
# 
# # Export: CSV -------------------------------------------------------------
# mcsv_w(dcast_MargRealGDPgrowth.ts,
#        aggr_composition_real_inputs_EE.ts,
#        dcast_grossOperatingSurplus_vs_profitMarginIndicator.ts,
#        dcast_melt_sectoral.markup_EE.ts,
#        mean_markup_EE.ts,comp_tradables_EE.ts,
#        comp_non_tradables_EE.ts,
#        dcast_B2GB3G_comp.ts,
#        dir="C:/Users/nreigl/Documents/projects/enterprise profitability/output")
# 
# 
# # Export: TikC ------------------------------------------------------------
# 
# ?tikz
# tikz("~/projects/enterprise profitability/presentation/Comp_B2G_incSK_out1.tex",  standAlone = F, sanitize =T)
# plot(Comp_B2G_incSK_out1)
# dev.off()                   # must turn device off to complete .tex file
# 
# 
# grossOperatingSurplus_vs_profitMarginIndicator
# plot_MargRealGDPgrowth
# p1
# plot_sectoral.markup_EE.ts
# p1_tradables
# p1_non_tradables
# plot_B2GB3G_comp.ts
# 
# 
# tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/grossOperatingSurplus_vs_profitMarginIndicator.tex",  standAlone = F, sanitize =T)
# plot(grossOperatingSurplus_vs_profitMarginIndicator)
# dev.off()                   # must turn device off to complete .tex file
# 
# tikz("~/projects/enterprise profitability/presentation/plot_MargRealGDPgrowth.tex",  standAlone = F, sanitize =T)
# plot(plot_MargRealGDPgrowth)
# dev.off()                   # must turn device off to complete .tex file
# 
# tikz("~/projects/enterprise profitability/presentation/p1.tex",  standAlone = F, sanitize =T)
# plot(p1)
# dev.off()                   # must turn device off to complete .tex file
# 
# 
# 
# tikz("~/projects/enterprise profitability/presentation/plot_sectoral.markup_EE.ts.tex",  standAlone = F, sanitize =T)
# plot(plot_sectoral.markup_EE.ts)
# dev.off()                   # must turn device off to complete .tex file
# 
# 
# tikz("~/projects/enterprise profitability/presentation/p1_tradables.tex",  standAlone = F, sanitize =T)
# plot(p1_tradables)
# dev.off()                   # must turn device off to complete .tex file
# 
# tikz("~/projects/enterprise profitability/presentation/p1_non_tradables.tex",  standAlone = F, sanitize =T)
# plot(p1_non_tradables)
# dev.off()                   # must turn device off to complete .tex file
# 
# tikz("~/projects/enterprise profitability/presentation/plot_B2GB3G_comp.ts.tex",  standAlone = F, sanitize =T)
# plot(plot_B2GB3G_comp.ts)
# dev.off()                   # must turn device off to complete .tex file


# 3) other variables ------------------------------------------------------

# Industrial countries' effective exchange rates including new Member States - quarterly data [ert_eff_ic_q]
## http://ec.europa.eu/eurostat/cache/metadata/en/ert_eff_esms.htm
neer <- get_eurostat("ert_eff_ic_q", time_format = "raw")
str(neer)

neer_EE<-neer %>% 
  filter (geo %in% c("EE")) %>%
  filter (ert %in% c("NEER28")) # Nominal Effective Exchange Rate - 28 trading partners  
  # arrange(desc(time)) # sort time column

head(neer_EE, 50)
View (neer_EE)

#  Unemployment rate by sex and age - quarterly average, % [une_rt_q]
## http://ec.europa.eu/eurostat/cache/metadata/en/une_esms.htm
unemp <- get_eurostat("une_rt_q", time_format = "raw")
str(unemp)

unemp_EE<-unemp %>% 
  filter (geo %in% c("EE")) %>%
  filter (s_adj %in% c("SA")) %>% 
  filter (age == "TOTAL") %>% 
  filter (sex == "T") %>% 
  arrange(desc(time)) # sort time column

View(unemp_EE)

#  Long-term unemployment by sex - quarterly average, % [une_ltu_q]
ltunemp <- get_eurostat("une_ltu_q", time_format = "raw")
str(ltunemp)

ltunemp_EE<-ltunemp %>% 
  filter (geo %in% c("EE")) %>%
  # filter (s_adj %in% c("NA", "SA", "TREND")) %>% 
  filter(indic_em == "LTU_UNE_RT") %>% 
  filter (sex == "T")

head(ltunemp_EE, 50)
View (ltunemp_EE)

#  Long-term unemployment by sex - quarterly average, % [une_ltu_q]
ltunemp <- get_eurostat("une_ltu_q", time_format = "raw")
str(ltunemp)

ltunemp_EE<-ltunemp %>% 
  filter (geo %in% c("EE")) %>%
  # filter (s_adj %in% c("NA", "SA", "TREND")) %>% 
  filter(indic_em == "LTU_UNE_RT") %>% 
  filter (sex == "T")

#  Money market interest rates - quarterly data [irt_h_mr3_q] pre Euro Area
mmrate <- get_eurostat("irt_h_mr3_q", time_format = "raw")

mmrate_EE<-mmrate %>% 
  filter (geo %in% c("EE")) %>%
  filter(intrt == "3MR")

mmrate_EE<-na.omit(mmrate_EE)

#  Money market interest rates - quarterly data [irt_h_mr3_q]  Euro Area  
EAmmrate <- get_eurostat("irt_st_q", time_format = "raw")
str(EAmmrate)

mmrate_EA<-EAmmrate %>% 
  filter (geo %in% c("EA")) %>%
  filter(intrt == "MAT_M03")

mmrate_EA$values[match(mmrate_EE$time,mmrate_EA$time)]<-mmrate_EE$values # Conditional merge/replacement 
mmrateEE <-na.omit(mmrate_EA)
mmrateEE<-mmrateEE %>% 
  arrange(desc(time)) # sort time column



# HICP (2005 = 100) - monthly data (index) [prc_hicp_midx]
HICP <- get_eurostat("prc_hicp_midx", time_format = "raw")
str(HICP)

HICPcore_EE<-HICP %>% 
  filter (geo %in% c("EE")) %>%
  filter(coicop == "TOT_X_NRG_FOOD") %>% # Overall index excluding energy, food, alcohol and tobacco 
  arrange(desc(time))

# Process the time series
## transform everything into a ts object
### profit margin
profitmrg.ts<-profit_margin_indicator1.df$profit_margin_indicator
### Nominal exchange rate
neerEE.ts<-ts(neer_EE$values, start = c(1994,1), frequency = 4)
### short-term money market rate
mmrateEE.ts<-ts(mmrateEE$values, start = c(1990,1), frequency = 4)
### core inflation index
Month_HICPcore_EE.ts<-na.omit(ts(HICPcore_EE$values, start = c(1996,1), frequency = 12))
Month_HICPcore_EE.df<-Month_HICPcore_EE.ts
manip_HICPcore_EE.df<-Month_HICPcore_EE.df[seq(1, length (Month_HICPcore_EE.df),3)] # we deal with monthly index values --> need to transform the series by taking every third end of month value)
quarter_HICPcore_EE.ts<-ts(manip_HICPcore_EE.df, start=c(1997,4), frequency = 4)
core.ts<-quarter_HICPcore_EE.ts

# merge frames
system1<-cbind(neerEE.ts,mmrateEE.ts,profitmrg.ts,core.ts)
system1_balanced.ts<-na.omit(system1)
system1_balanced.df<-as.data.frame(system1_balanced.ts)

system1_balanced.ts

colnames(system1_balanced.df)

markup_project_full.matrix <- read.csv("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/enterprise profitability/markup_project_full matrix.csv", stringsAsFactors=FALSE)
full_matrix.ts<-ts(markup_project_full.matrix[,c(2:6)], start = c(1995,1), frequency = 4) # convert to time series object

# close the graphics device
dev.list()
dev.off()
devAskNewPage(FALSE)
options(device.ask.default = FALSE)

full_matrix.ts


# 4) sequential testing ---------------------------------------------------
## 4.1 Decompose a time series into seasonal, trend and irregular components using moving averages
plot(decompose(full_matrix.ts[, "LEENEER"]))
plot(decompose(full_matrix.ts[, "EEMMRATE"]))
plot(decompose(full_matrix.ts[, "LEEPROFITMRG"]))
plot(decompose(full_matrix.ts[, "LCORE"]))

## 4.2 Plot besides each other
plot(full_matrix.ts)
# IRF TikZ Latex Output
tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/full_matrix.tex",
     standAlone = F, sanitize =T)
plot(full_matrix.ts, col = "steelblue")                  
dev.off()

## 4.3 Fit an AR process and estimate the autocovariance and autocorrelation function
order_neer<-ar((full_matrix.ts[, "LEENEER"]), method = "ols", aic = T)
order_neer$order
acf(full_matrix.ts[, "LEENEER"], lag.max=30)
pacf(full_matrix.ts[, "LEENEER"], lag.max=30)

order_mmrate<-ar((full_matrix.ts[, "EEMMRATE"]), method = "ols", aic = T)
order_mmrate$order
acf(full_matrix.ts[, "EEMMRATE"], lag.max=30)
pacf(full_matrix.ts[, "EEMMRATE"], lag.max=30)

order_profitmrg<-ar((full_matrix.ts[, "LEEPROFITMRG"]), method = "ols", aic = T)
order_profitmrg$order
acf(full_matrix.ts[, "LEEPROFITMRG"], lag.max=30)
pacf(full_matrix.ts[, "LEEPROFITMRG"], lag.max=30)

order_core<-ar((full_matrix.ts[, "LCORE"]), method = "ols", aic = T)
order_core$order
acf(full_matrix.ts[, "LCORE"], lag.max=30)
pacf(full_matrix.ts[, "LCORE"], lag.max=30)

## 4.4 Test for stationary/ unit root sequentionally
### 4.4.1 NEER
#### ADF
plot(full_matrix.ts[,"LEENEER"])
adf_p1C_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "drift", lags = 1)) # intercept
adf_p2C_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "drift", lags = 2)) #  intercept
adf_p3C_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "drift", lags = 3)) #  intercept
adf_p4C_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "drift", lags = 4)) # intercept
adf_p3_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "none", lags = 3)) # none
adf_p1CT_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "drift", lags = 3)) # drift and trend
adf_p3CT_NEER<-(ur.df(full_matrix.ts[, "LEENEER"], type = "drift", lags = 3)) # drift and trend

plot(adf_p1C_NEER)
plot(adf_p2C_NEER)
plot(adf_p3C_NEER)
plot(adf_p3T_NEER)
plot(adf_p4CT_NEER)

SUMadf_p1CT_NEER<-summary(adf_p1CT_NEER)
SUMadf_p3CT_NEER<-summary(adf_p3CT_NEER)
SUMadf_p2CT_NEER<-summary(adf_p2CT_NEER)
SUMadf_p3_NEER<-summary(adf_p3_NEER)
SUMadf_p3T_NEER<-summary(adf_p3T_NEER)
SUMadf_p4CT_NEER<-summary(adf_p4CT_NEER)

SUMadf_p3CT_NEER # best fit
SUMadf_p2CT_NEER # test regression with only two lagged endogenous variables does not suffice to achieve serially uncorr. errors
SUMadf_p3_NEER 
SUMadf_p3T_NEER
SUMadf_p1CT_NEER
SUMadf_p4CT_NEER ## including a fourth lag turns out to be insignificant

#### Stationarity inducing transformation: first diff (d) or diff log (dl) 
adf_p2T_dNEER<-(ur.df(diff(full_matrix.ts[, "LEENEER"],4), type = "drift", lags = 2)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p2T_dNEER)
adf_p2_dlNEER<-(ur.df(diff(log(full_matrix.ts[, "LEENEER"]),4), type = "none", lags = 2)) # trend and intercept (yoy diff (log) --> 4 lags)
summary(adf_p2_dlNEER)

#### KPSS
kpss_NEER <- ur.kpss(full_matrix.ts[, "LEENEER"], type="tau", lags="short") # trend  ("mu"), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_NEER)
kpss_dNEER <- ur.kpss(diff(full_matrix.ts[, "LEENEER"],4), type="mu", lags="short") # trend ("mu"), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dNEER)
kpss_dlNEER <-ur.kpss(diff(log(full_matrix.ts[, "LEENEER"]),4), type="mu", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dlNEER)


#### Export as Latex files
## Version dev()
class(SUMadf_p3CT_NEER@testreg$coefficients) <- "matrix"
latex(SUMadf_p3CT_NEER@testreg$coefficients,center="centering",caption="de", digits = 3, 
      #,file="" # onscreen or dev to file
)
## On screen
xtable(SUMadf_p3CT_NEER@testreg$coefficients, caption = "Nominal exchange rate", centering="centering", label = "NEER", digits = 3)

# ADF output table: 
## Put the test-statistics and the critical values together
expADF_NEER <- cbind(t(SUMadf_p3CT_NEER@teststat), SUMadf_p3CT_NEER@cval)
xtable(expADF_NEER, caption = "ADF test: ", centering="centering", label = "NULL", digits = 3)
diffexpADF_NEER <- cbind(t(adf_p2T_dNEER@teststat), adf_p2T_dNEER@cval)
xtable(diffexpADF_NEER, caption = "ADF test: ", centering="centering", label = "NULL", digits = 3)

# KPSS output table: 
## Put the test-statistics and the critical values together
SUMkpss_NEER<-summary(kpss_NEER)
expKPSS_NEER <- cbind(t(SUMkpss_NEER@teststat), SUMkpss_NEER@cval)
xtable(expKPSS_NEER, caption = "KPSS test: ", centering="centering", label = "NULL", digits = 3)


### 4.4.2 MMRATE
#### ADF
plot(full_matrix.ts[, "EEMMRATE"])
adf_p1CT_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "trend", lags = 1)) # trend and intercept
adf_p2CT_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "trend", lags = 2)) # trend and intercept
adf_p3CT_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "trend", lags = 3)) # trend and intercept
adf_p4CT_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "trend", lags = 4)) # trend and intercept
adf_p5CT_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "trend", lags = 5)) # trend and intercept
adf_p3_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "none", lags = 3)) # none
adf_p3T_MMRATE<-(ur.df(full_matrix.ts[, "EEMMRATE"], type = "drift", lags = 3)) # drift only

plot(adf_p1CT_MMRATE)
plot(adf_p2CT_MMRATE)
plot(adf_p3CT_MMRATE)
plot(adf_p3_MMRATE)
plot(adf_p4CT_MMRATE)
plot(adf_p5CT_MMRATE)

SUMadf_p1CT_MMRATE<-summary(adf_p1CT_MMRATE)
SUMadf_p3CT_MMRATE<-summary(adf_p3CT_MMRATE)
SUMadf_p2CT_MMRATE<-summary(adf_p2CT_MMRATE)
SUMadf_p3_MMRATE<-summary(adf_p3_MMRATE)
SUMadf_p3T_MMRATE<-summary(adf_p3T_MMRATE)
SUMadf_p4CT_MMRATE<-summary(adf_p4CT_MMRATE)
SUMadf_p5CT_MMRATE<-summary(adf_p5CT_MMRATE)

SUMadf_p3CT_MMRATE # test regression with only two lagged endogenous variables does not suffice to achieve serially uncorr. errors
SUMadf_p2CT_MMRATE 
SUMadf_p3_MMRATE 
SUMadf_p3T_MMRATE
SUMadf_p1CT_MMRATE
SUMadf_p4CT_MMRATE ## # best fit
SUMadf_p5CT_MMRATE ## including a fith lag turns out to be insignificant

#### Stationarity inducing transformation: first diff (d) or diff log (dl) 
adf_p3T_dMMRATE<-(ur.df(diff(full_matrix.ts[, "EEMMRATE"],4), type = "drift", lags = 3)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p3T_dMMRATE)
adf_p3_dlMMRATE<-(ur.df(diff(log(full_matrix.ts[, "EEMMRATE"]),4), type = "none", lags = 3)) # trend and intercept (yoy diff (log) --> 4 lags)
summary(adf_p3_dlMMRATE)


#### KPSS
kpss_MMRATE <- ur.kpss(full_matrix.ts[, "EEMMRATE"], type="tau", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_MMRATE)
kpss_dMMRATE <- ur.kpss(diff(full_matrix.ts[, "EEMMRATE"],4), type="mu", lags="short") # trend  ("mu"), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dMMRATE)
kpss_dlMMRATE <-ur.kpss(diff(log(full_matrix.ts[, "EEMMRATE"]),4), type="mu", lags="short") # trend  ("mu"), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dlMMRATE)


#### Export as Latex files
## Version dev()
class(SUMadf_p3CT_MMRATE@testreg$coefficients) <- "matrix"
latex(SUMadf_p3CT_MMRATE@testreg$coefficients,center="centering",caption="de", digits = 3, 
      #,file="" # onscreen or dev to file
)
## On screen
xtable(SUMadf_p3CT_MMRATE@testreg$coefficients, caption = "Short-term interest rate Estonia", centering="centering", label = "MMRATE", digits = 3)

# ADF output table: 
## Put the test-statistics and the critical values together
expADF_MMRATE <- cbind(t(SUMadf_p3CT_MMRATE@teststat), SUMadf_p3CT_MMRATE@cval)
xtable(expADF_MMRATE, caption = "Short Term Interest Rate - ADF test: ", centering="centering", label = "NULL", digits = 3)
diffexpADF_MMRATE <- cbind(t(adf_p3T_dMMRATE@teststat), adf_p3T_dMMRATE@cval)
xtable(diffexpADF_MMRATE, caption = "Short Term Interest Rate - ADF test: ", centering="centering", label = "NULL", digits = 3)

# KPSS output table: 
## Put the test-statistics and the critical values together
SUMkpss_MMRATE<-summary(kpss_MMRATE)
expKPSS_MMRATE <- cbind(t(SUMkpss_MMRATE@teststat), SUMkpss_MMRATE@cval)
xtable(expKPSS_MMRATE, caption = "Short Term Interest Rate - KPSS test: ", centering="centering", label = "NULL", digits = 3)


### 4.4.3 PROFITMRG
#### ADF
plot(full_matrix.ts[, "LEEPROFITMRG"])
adf_p1C_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "drift", lags = 1)) # intercept
adf_p2C_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "drift", lags = 2)) # intercept
adf_p3C_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "drift", lags = 3)) # intercept
adf_p4C_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "drift", lags = 4)) # intercept
adf_p1CT_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "trend", lags = 1)) # trend and intercept
adf_p2CT_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "trend", lags = 2)) # trend and intercept
adf_p3CT_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "trend", lags = 3)) # trend and intercept
adf_p4CT_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "trend", lags = 4)) # trend and intercept
adf_p1_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "none", lags = 1)) # none
adf_p2_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "none", lags = 2)) # none
adf_p3_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "none", lags = 3)) # none
adf_p4_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "none", lags = 4)) # none
adf_p3T_lPROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG"], type = "drift", lags = 3)) # drift only

plot(adf_p1C_lPROFITMRG)
plot(adf_p2C_lPROFITMRG)
plot(adf_p3C_lPROFITMRG)
plot(adf_p3cT_lPROFITMRG)
plot(adf_p4CT_lPROFITMRG)


SUMadf_p1CT_lPROFITMRG<-summary(adf_p1CT_lPROFITMRG)
SUMadf_p3CT_lPROFITMRG<-summary(adf_p3CT_lPROFITMRG)
SUMadf_p2CT_lPROFITMRG<-summary(adf_p2CT_lPROFITMRG)
SUMadf_p3_lPROFITMRG<-summary(adf_p3_lPROFITMRG)
SUMadf_p3T_lPROFITMRG<-summary(adf_p3T_lPROFITMRG)
SUMadf_p4CT_lPROFITMRG<-summary(adf_p4CT_lPROFITMRG)

SUMadf_p1CT_lPROFITMRG
SUMadf_p2CT_lPROFITMRG
SUMadf_p3CT_lPROFITMRG
SUMadf_p4CT_lPROFITMRG

SUMadf_p1C_lPROFITMRG<-summary(adf_p1C_lPROFITMRG)
SUMadf_p2C_lPROFITMRG<-summary(adf_p2C_lPROFITMRG)
SUMadf_p3C_lPROFITMRG<-summary(adf_p3C_lPROFITMRG)
SUMadf_p4C_lPROFITMRG<-summary(adf_p4C_lPROFITMRG)

SUMadf_p1C_lPROFITMRG
SUMadf_p2C_lPROFITMRG
SUMadf_p3C_lPROFITMRG
SUMadf_p4C_lPROFITMRG

SUMadf_p1_lPROFITMRG<-summary(adf_p1_lPROFITMRG) 
SUMadf_p2_lPROFITMRG<-summary(adf_p2_lPROFITMRG)
SUMadf_p3_lPROFITMRG<-summary(adf_p3_lPROFITMRG)
SUMadf_p4_lPROFITMRG<-summary(adf_p4_lPROFITMRG)


SUMadf_p1_lPROFITMRG 
SUMadf_p2_lPROFITMRG
SUMadf_p3_lPROFITMRG
SUMadf_p4_lPROFITMRG


#### Stationarity inducing transformation: first diff (d)
adf_p1_dlPROFITMRG<-(ur.df(diff((full_matrix.ts[, "LEEPROFITMRG"]),4, diff = 1), type = "none", lags = 1)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p1_dlPROFITMRG)
plot(adf_p1_dlPROFITMRG)

adf_p2_dlPROFITMRG<-(ur.df(diff((full_matrix.ts[, "LEEPROFITMRG"]),4, diff = 1), type = "none", lags = 2)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p2_dlPROFITMRG)
plot(adf_p2_dlPROFITMRG)

adf_p3_dlPROFITMRG<-(ur.df(diff((full_matrix.ts[, "LEEPROFITMRG"]),4, diff = 1), type = "none", lags = 3)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p3_dlPROFITMRG)
plot(adf_p1_dlPROFITMRG)



#### KPSS
test<-exp(full_matrix.ts[, "LEEPROFITMRG"])

# constant with linear trend
kpss_p4_lPROFITMRG_tau <- ur.kpss(full_matrix.ts[, "LEEPROFITMRG"], type="tau", use.lag = 4) # intercept ("mu")
summary(kpss_p4_lPROFITMRG_tau)

# constant
kpss_p4_lPROFITMRG_mu <- ur.kpss(full_matrix.ts[, "LEEPROFITMRG"], type="mu", use.lag = 4) # intercept ("mu")
summary(kpss_p4_lPROFITMRG_mu)

kpss_p1_dlPROFITMRG <- ur.kpss(diff(full_matrix.ts[, "LEEPROFITMRG"],4,diff = 1), use.lag = 1) # 
summary(kpss_p1_dlPROFITMRG)

kpss_p2_dlPROFITMRG <- ur.kpss(diff(full_matrix.ts[, "LEEPROFITMRG"],4), use.lag = 2) # 
summary(kpss_p2_dlPROFITMRG)

kpss_p3_dlPROFITMRG <- ur.kpss(diff(full_matrix.ts[, "LEEPROFITMRG"],4), use.lag = 3) # 
summary(kpss_p3_dlPROFITMRG)

?ur.kpss
?ur.df

#### Export as Latex files
## Version dev()
class(SUMadf_p4C_lPROFITMRG@testreg$coefficients) <- "matrix"
latex(SUMadf_p4C_lPROFITMRG@testreg$coefficients,center="centering",caption="de", digits = 3, 
      #,file="" # onscreen or dev to file
)
## On screen
xtable(SUMadf_p4C_lPROFITMRG@testreg$coefficients, caption = "Profit Margin Estonia", centering="centering", label = "PROFITMRG", digits = 3)

# ADF output table: 
## Put the test-statistics and the critical values together
expADF_PROFITMRG_1 <- cbind(t((summary(adf_p4C_lPROFITMRG)@teststat)), (summary(adf_p4C_lPROFITMRG))@cval)
expADF_PROFITMRG_2 <- cbind(t((summary(adf_p3_dlPROFITMRG)@teststat)), (summary(adf_p3_dlPROFITMRG))@cval)
# expADF_PROFITMRG_3 <- cbind(t((summary(adf_p2_dlPROFITMRG)@teststat)), (summary(adf_p2_dlPROFITMRG))@cval)
# expADF_PROFITMRG_4 <- cbind(t((summary(adf_p1_dlPROFITMRG)@teststat)), (summary(adf_p1_dlPROFITMRG))@cval)

expADF_PROFITMRG<-rbind(expADF_PROFITMRG_1,  
      expADF_PROFITMRG_2  
      # expADF_PROFITMRG_3,  
      # expADF_PROFITMRG_4
      )
expADF_PROFITMRG

# xtable(expADF_PROFITMRG, caption = "Profit Margin - ADF test: ", centering="centering", label = "NULL", digits = 3)

# KPSS output table: 
## Put the test-statistics and the critical values together
expKPSS_PROFITMRG_1 <- cbind(t((summary(kpss_p4_lPROFITMRG_tau)@teststat)), (summary(kpss_p4_lPROFITMRG_tau))@cval)
expKPSS_PROFITMRG_2 <- cbind(t((summary(kpss_p4_lPROFITMRG_mu)@teststat)), (summary(kpss_p4_lPROFITMRG_mu))@cval)
expKPSS_PROFITMRG_3 <- cbind(t((summary(kpss_p3_dlPROFITMRG)@teststat)), (summary(kpss_p3_dlPROFITMRG))@cval)

expKPSS_PROFITMRG<-rbind(expKPSS_PROFITMRG_1,
      expKPSS_PROFITMRG_2,
      expKPSS_PROFITMRG_3
)

expKPSS_PROFITMRG.df<-as.data.frame(expKPSS_PROFITMRG, row.names = c("1", "2", "3"))
# reoder
expKPSS_PROFITMRG.df<-expKPSS_PROFITMRG.df[,-4]
expKPSS_PROFITMRG.df<-expKPSS_PROFITMRG.df[,c(1,4,3,2)]
colnames(expKPSS_PROFITMRG.df)[1] <- "statistic"
stat.test<-rbind(expADF_PROFITMRG, expKPSS_PROFITMRG.df)

xtable(stat.test, caption = "Unit root tests on profit margin indicator", centering="centering", label = "NULL", digits = 3)


### 4.4.4 CORE
#### ADF
plot(full_matrix.ts[, "LCORE"])
adf_p1CT_CORE<-(ur.df(full_matrix.ts[,"LCORE"], type = "trend", lags = 1)) # trend and intercept
adf_p2CT_CORE<-(ur.df(full_matrix.ts[, "LCORE"], type = "trend", lags = 2)) # trend and intercept
adf_p3CT_CORE<-(ur.df(full_matrix.ts[, "LCORE"], type = "trend", lags = 3)) # trend and intercept
adf_p4CT_CORE<-(ur.df(full_matrix.ts[, "LCORE"], type = "trend", lags = 4)) # trend and intercept
adf_p5CT_CORE<-(ur.df(full_matrix.ts[, "LCORE"], type = "trend", lags = 5)) # trend and intercept

adf_p3_CORE<-(ur.df(full_matrix.ts[, "LCORE"], type = "none", lags = 3)) # none
adf_p3T_CORE<-(ur.df(full_matrix.ts[, "LCORE"], type = "drift", lags = 3)) # drift only

plot(adf_p1CT_CORE)
plot(adf_p2CT_CORE)
plot(adf_p3CT_CORE)
plot(adf_p3_CORE)
plot(adf_p4CT_CORE)
plot(adf_p5CT_CORE)


SUMadf_p1CT_CORE<-summary(adf_p1CT_CORE)
SUMadf_p3CT_CORE<-summary(adf_p3CT_CORE)
SUMadf_p2CT_CORE<-summary(adf_p2CT_CORE)
SUMadf_p3_CORE<-summary(adf_p3_CORE)
SUMadf_p3T_CORE<-summary(adf_p3T_CORE)
SUMadf_p4CT_CORE<-summary(adf_p4CT_CORE)
SUMadf_p5CT_CORE<-summary(adf_p5CT_CORE)


SUMadf_p3CT_CORE # best fit
SUMadf_p2CT_CORE 
SUMadf_p3_CORE 
SUMadf_p3T_CORE
SUMadf_p1CT_CORE
SUMadf_p4CT_CORE 
SUMadf_p5CT_CORE 

#### Stationarity inducing transformation: first diff (d) or diff log (dl) 
adf_p2T_dCORE<-(ur.df(diff(full_matrix.ts[, "LCORE"],4), type = "drift", lags = 2)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p2T_dCORE)
adf_p2_dlCORE<-(ur.df(diff(log(full_matrix.ts[, "LCORE"]),4), type = "drift", lags = 2)) # trend and intercept (yoy diff (log) --> 4 lags)
summary(adf_p2_dlCORE)

#### KPSS
kpss_CORE <- ur.kpss(full_matrix.ts[, "LCORE"], type="mu", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_CORE)
kpss_dCORE <- ur.kpss(diff(full_matrix.ts[, "LCORE"],4), type="tau", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dCORE)
kpss_dlCORE <-ur.kpss(diff(log(full_matrix.ts[, "LCORE"]),4), type="tau", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dlCORE)


#### Export as Latex files
## Version dev()
class(SUMadf_p3CT_CORE@testreg$coefficients) <- "matrix"
latex(SUMadf_p3CT_CORE@testreg$coefficients,center="centering",caption="de", digits = 3, 
      #,file="" # onscreen or dev to file
)
## On screen
xtable(SUMadf_p3CT_CORE@testreg$coefficients, caption = "Core Inflation Estonia", centering="centering", label = "CORE", digits = 3)

# ADF output table: 
## Put the test-statistics and the critical values together
expADF_CORE <- cbind(t(SUMadf_p3CT_CORE@teststat), SUMadf_p3CT_CORE@cval)
xtable(expADF_CORE, caption = "Core Inflation - ADF test: ", centering="centering", label = "NULL", digits = 3)
# KPSS output table: 
## Put the test-statistics and the critical values together
SUMkpss_CORE<-summary(kpss_CORE)
expKPSS_CORE <- cbind(t(SUMkpss_CORE@teststat), SUMkpss_CORE@cval)
xtable(expKPSS_CORE, caption = "Core Inflation - KPSS test: ", centering="centering", label = "NULL", digits = 3)


adf_p3CT_NEER@teststat[,1]
adf_p3CT_NEER@cval
adf_p3CT_NEER@cval[1,]

## Complete ouput table of the ADF and KPSS test
expADF_NEER <- cbind((adf_p3CT_NEER@teststat[,1]),t(adf_p3CT_NEER@cval[1,]))
diffexpADF_NEER <- cbind((adf_p2T_dNEER@teststat[,1]),t(adf_p2T_dNEER@cval[1,]))
expADF_MMRATE <- cbind((adf_p3CT_MMRATE@teststat[,1]),t(adf_p3CT_MMRATE@cval[1,]))
diffexpADF_MMRATE <- cbind((adf_p3T_dMMRATE@teststat[,1]),t(adf_p3T_dMMRATE@cval[1,]))
expADF_PROFITMRG <- cbind((adf_p3CT_PROFITMRG@teststat[,1]),t(adf_p3CT_PROFITMRG@cval[1,]))
diffexpADF_PROFITMRG <- cbind(adf_p3CT_dPROFITMRG@teststat[,1], t(adf_p3CT_dPROFITMRG@cval[1,]))
ddiffexpADF_PROFITMRG <- cbind(adf_p3CT_ddPROFITMRG@teststat[,1], t(adf_p3CT_ddPROFITMRG@cval[1,]))
expADF_CORE <- cbind(adf_p3CT_CORE@teststat[,1],t(adf_p3CT_CORE@cval[1,]))
diffexpADF_CORE <- cbind(adf_p2T_dCORE@teststat[,1],t(adf_p2T_dCORE@cval[1,]))

adf_matrix<-rbind(expADF_NEER,diffexpADF_NEER, expADF_MMRATE, diffexpADF_MMRATE, expADF_PROFITMRG,diffexpADF_PROFITMRG, ddiffexpADF_PROFITMRG, expADF_CORE, diffexpADF_CORE)
adf_matrix.df<-as.data.frame(adf_matrix)

# round function for dataframes that contain non-numeric data
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

round_df(adf_matrix, 3)
adf_matrix.df <- cbind(Variable = rownames(adf_matrix.df), adf_matrix.df)
xtable(adf_matrix.df, caption = "ADF tests for Estonian data", centering="centering", label = "NULL", digits = 3)

expKPSS_NEER <- cbind(t(kpss_NEER@teststat), kpss_NEER@cval)
diffexpKPSS_NEER <- cbind(t(kpss_dNEER@teststat), kpss_dNEER@cval)
expKPSS_MMRATE <- cbind(t(kpss_MMRATE@teststat), kpss_MMRATE@cval)
diffexpKPSS_MMRATE <- cbind(t(kpss_dMMRATE@teststat), kpss_dMMRATE@cval)
expKPSS_PROFITMRG <- cbind(t(kpss_PROFITMRG@teststat), kpss_PROFITMRG@cval)
diffexpKPSS_PROFITMRG <- cbind(t(kpss_dPROFITMRG@teststat), kpss_dPROFITMRG@cval)
expKPSS_CORE <- cbind(t(kpss_CORE@teststat), kpss_CORE@cval)
diffexpKPSS_CORE <- cbind(t(kpss_dCORE@teststat), kpss_dCORE@cval)

kpss_matrix<-rbind(
  expKPSS_NEER,  
  diffexpKPSS_NEER, 
  expKPSS_MMRATE, 
  diffexpKPSS_MMRATE, 
  expKPSS_PROFITMRG,  
  diffexpKPSS_PROFITMRG, 
  expKPSS_CORE, 
  diffexpKPSS_CORE)  

kpss_matrix.df<-as.data.frame(kpss_matrix)
round_df(kpss_matrix, 3)
kpss_matrix.df <- cbind(Variable = rownames(kpss_matrix.df), kpss_matrix.df)
xtable(kpss_matrix, caption = "KPSS tests for Estonian data", centering="centering", label = "NULL", digits = 3)

### 4.4.3 PROFITMRG
#### ADF
plot(full_matrix.ts[, "LEEPROFITMRG "])
adf_p1CT_PROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG "], type = "trend", lags = 1)) # trend and intercept
adf_p2CT_PROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG "], type = "trend", lags = 2)) # trend and intercept
adf_p3CT_PROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG "], type = "trend", lags = 3)) # trend and intercept
adf_p4CT_PROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG "], type = "trend", lags = 4)) # trend and intercept
adf_p3_PROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG "], type = "none", lags = 3)) # none
adf_p3T_PROFITMRG<-(ur.df(full_matrix.ts[, "LEEPROFITMRG "], type = "drift", lags = 3)) # drift only

plot(adf_p1CT_PROFITMRG)
plot(adf_p2CT_PROFITMRG)
plot(adf_p3CT_PROFITMRG)
plot(adf_p3T_PROFITMRG)
plot(adf_p4CT_PROFITMRG)

SUMadf_p1CT_PROFITMRG<-summary(adf_p1CT_PROFITMRG)
SUMadf_p3CT_PROFITMRG<-summary(adf_p3CT_PROFITMRG)
SUMadf_p2CT_PROFITMRG<-summary(adf_p2CT_PROFITMRG)
SUMadf_p3_PROFITMRG<-summary(adf_p3_PROFITMRG)
SUMadf_p3T_PROFITMRG<-summary(adf_p3T_PROFITMRG)
SUMadf_p4CT_PROFITMRG<-summary(adf_p4CT_PROFITMRG)

SUMadf_p3CT_PROFITMRG 
SUMadf_p2CT_PROFITMRG # second best fit 
SUMadf_p3_PROFITMRG  
SUMadf_p3T_PROFITMRG
SUMadf_p1CT_PROFITMRG # best fit
SUMadf_p4CT_PROFITMRG 

#### Stationarity inducing transformation: first diff (d) or diff log (dl) 
adf_p3CT_dPROFITMRG<-(ur.df(diff(full_matrix.ts[, "LEEPROFITMRG "],4, diff = 1), type = "trend", lags = 3)) # trend and intercept (yoy diff --> 4 lags)
plot(adf_p3CT_dPROFITMRG)
summary(adf_p3CT_dPROFITMRG)
adf_p3CT_ddPROFITMRG<-(ur.df(diff(full_matrix.ts[, "LEEPROFITMRG "],4, diff = 2), type = "trend", lags = 3)) # trend and intercept (yoy diff --> 4 lags)
plot(adf_p3CT_ddPROFITMRG)
summary(adf_p3CT_ddPROFITMRG)
adf_p3CT_dlPROFITMRG<-(ur.df(diff(log(full_matrix.ts[, "LEEPROFITMRG "]),4), type = "trend", lags = 3)) # trend and intercept (yoy diff (log) --> 4 lags)
plot(adf_p3CT_dlPROFITMRG)
summary(adf_p3CT_dlPROFITMRG)

#### KPSS
kpss_PROFITMRG <- ur.kpss(full_matrix.ts[, "LEEPROFITMRG "], type="tau", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_PROFITMRG)
kpss_dPROFITMRG <- ur.kpss(diff(full_matrix.ts[, "LEEPROFITMRG "],4), type="mu", lags="short") # trend  ("mu"), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dPROFITMRG)
kpss_dlPROFITMRG <-ur.kpss(diff(log(full_matrix.ts[, "LEEPROFITMRG "]),4), type="mu", lags="short") # trend and intercept ("mu"), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dlPROFITMRG)


#### Export as Latex files
## Version dev()
class(SUMadf_p3CT_PROFITMRG@testreg$coefficients) <- "matrix"
latex(SUMadf_p3CT_PROFITMRG@testreg$coefficients,center="centering",caption="de", digits = 3, 
      #,file="" # onscreen or dev to file
)
## On screen
xtable(SUMadf_p3CT_PROFITMRG@testreg$coefficients, caption = "Profit Margin Estonia", centering="centering", label = "PROFITMRG", digits = 3)

# ADF output table: 
## Put the test-statistics and the critical values together
expADF_PROFITMRG <- cbind(t(SUMadf_p3CT_PROFITMRG@teststat), SUMadf_p3CT_PROFITMRG@cval)
xtable(expADF_PROFITMRG, caption = "Profit Margin - ADF test: ", centering="centering", label = "NULL", digits = 3)
diffexpADF_PROFITMRG <- cbind(t(summary(adf_p3CT_ddPROFITMRG)@teststat), summary(adf_p3CT_ddPROFITMRG)@cval)

# KPSS output table: 
## Put the test-statistics and the critical values together
SUMkpss_PROFITMRG<-summary(kpss_PROFITMRG)
expKPSS_PROFITMRG <- cbind(t(SUMkpss_PROFITMRG@teststat), SUMkpss_PROFITMRG@cval)
xtable(expKPSS_PROFITMRG, caption = "Profit Margin - KPSS test: ", centering="centering", label = "NULL", digits = 3)


### 4.4.4 CORE
#### ADF
plot(full_matrix.ts[, " LCORE"])
adf_p1CT_CORE<-(ur.df(full_matrix.ts[," LCORE"], type = "trend", lags = 1)) # trend and intercept
adf_p2CT_CORE<-(ur.df(full_matrix.ts[, " LCORE"], type = "trend", lags = 2)) # trend and intercept
adf_p3CT_CORE<-(ur.df(full_matrix.ts[, " LCORE"], type = "trend", lags = 3)) # trend and intercept
adf_p4CT_CORE<-(ur.df(full_matrix.ts[, " LCORE"], type = "trend", lags = 4)) # trend and intercept
adf_p5CT_CORE<-(ur.df(full_matrix.ts[, " LCORE"], type = "trend", lags = 5)) # trend and intercept

adf_p3_CORE<-(ur.df(full_matrix.ts[, " LCORE"], type = "none", lags = 3)) # none
adf_p3T_CORE<-(ur.df(full_matrix.ts[, " LCORE"], type = "drift", lags = 3)) # drift only

plot(adf_p1CT_CORE)
plot(adf_p2CT_CORE)
plot(adf_p3CT_CORE)
plot(adf_p3_CORE)
plot(adf_p4CT_CORE)
plot(adf_p5CT_CORE)


SUMadf_p1CT_CORE<-summary(adf_p1CT_CORE)
SUMadf_p3CT_CORE<-summary(adf_p3CT_CORE)
SUMadf_p2CT_CORE<-summary(adf_p2CT_CORE)
SUMadf_p3_CORE<-summary(adf_p3_CORE)
SUMadf_p3T_CORE<-summary(adf_p3T_CORE)
SUMadf_p4CT_CORE<-summary(adf_p4CT_CORE)
SUMadf_p5CT_CORE<-summary(adf_p5CT_CORE)


SUMadf_p3CT_CORE # best fit
SUMadf_p2CT_CORE 
SUMadf_p3_CORE 
SUMadf_p3T_CORE
SUMadf_p1CT_CORE
SUMadf_p4CT_CORE 
SUMadf_p5CT_CORE 

#### Stationarity inducing transformation: first diff (d) or diff log (dl) 
adf_p2T_dCORE<-(ur.df(diff(full_matrix.ts[, " LCORE"],4), type = "drift", lags = 2)) # trend and intercept (yoy diff --> 4 lags)
summary(adf_p2T_dCORE)
adf_p2_dlCORE<-(ur.df(diff(log(full_matrix.ts[, " LCORE"]),4), type = "drift", lags = 2)) # trend and intercept (yoy diff (log) --> 4 lags)
summary(adf_p2_dlCORE)

#### KPSS
kpss_CORE <- ur.kpss(full_matrix.ts[, " LCORE"], type="mu", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_CORE)
kpss_dCORE <- ur.kpss(diff(full_matrix.ts[, " LCORE"],4), type="tau", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dCORE)
kpss_dlCORE <-ur.kpss(diff(log(full_matrix.ts[, " LCORE"]),4), type="tau", lags="short") # trend and intercept ("tau), lags="short" sets the number of lags to \root 4 \of {4 \times (n/100)}
summary(kpss_dlCORE)


#### Export as Latex files
## Version dev()
class(SUMadf_p3CT_CORE@testreg$coefficients) <- "matrix"
latex(SUMadf_p3CT_CORE@testreg$coefficients,center="centering",caption="de", digits = 3, 
      #,file="" # onscreen or dev to file
)
## On screen
xtable(SUMadf_p3CT_CORE@testreg$coefficients, caption = "Core Inflation Estonia", centering="centering", label = "CORE", digits = 3)

# ADF output table: 
## Put the test-statistics and the critical values together
expADF_CORE <- cbind(t(SUMadf_p3CT_CORE@teststat), SUMadf_p3CT_CORE@cval)
xtable(expADF_CORE, caption = "Core Inflation - ADF test: ", centering="centering", label = "NULL", digits = 3)
# KPSS output table: 
## Put the test-statistics and the critical values together
SUMkpss_CORE<-summary(kpss_CORE)
expKPSS_CORE <- cbind(t(SUMkpss_CORE@teststat), SUMkpss_CORE@cval)
xtable(expKPSS_CORE, caption = "Core Inflation - KPSS test: ", centering="centering", label = "NULL", digits = 3)


adf_p3CT_NEER@teststat[,1]
adf_p3CT_NEER@cval
adf_p3CT_NEER@cval[1,]

## Complete ouput table of the ADF and KPSS test
expADF_NEER <- cbind((adf_p3CT_NEER@teststat[,1]),t(adf_p3CT_NEER@cval[1,]))
diffexpADF_NEER <- cbind((adf_p2T_dNEER@teststat[,1]),t(adf_p2T_dNEER@cval[1,]))
expADF_MMRATE <- cbind((adf_p3CT_MMRATE@teststat[,1]),t(adf_p3CT_MMRATE@cval[1,]))
diffexpADF_MMRATE <- cbind((adf_p3T_dMMRATE@teststat[,1]),t(adf_p3T_dMMRATE@cval[1,]))
expADF_PROFITMRG <- cbind((adf_p3CT_PROFITMRG@teststat[,1]),t(adf_p3CT_PROFITMRG@cval[1,]))
diffexpADF_PROFITMRG <- cbind(adf_p3CT_dPROFITMRG@teststat[,1], t(adf_p3CT_dPROFITMRG@cval[1,]))
ddiffexpADF_PROFITMRG <- cbind(adf_p3CT_ddPROFITMRG@teststat[,1], t(adf_p3CT_ddPROFITMRG@cval[1,]))
expADF_CORE <- cbind(adf_p3CT_CORE@teststat[,1],t(adf_p3CT_CORE@cval[1,]))
diffexpADF_CORE <- cbind(adf_p2T_dCORE@teststat[,1],t(adf_p2T_dCORE@cval[1,]))

adf_matrix<-rbind(expADF_NEER,diffexpADF_NEER, expADF_MMRATE, diffexpADF_MMRATE, expADF_PROFITMRG,diffexpADF_PROFITMRG, ddiffexpADF_PROFITMRG, expADF_CORE, diffexpADF_CORE)
adf_matrix.df<-as.data.frame(adf_matrix)

# round function for dataframes that contain non-numeric data
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

round_df(adf_matrix, 3)
adf_matrix.df <- cbind(Variable = rownames(adf_matrix.df), adf_matrix.df)
xtable(adf_matrix.df, caption = "ADF tests for Estonian data", centering="centering", label = "NULL", digits = 3)

expKPSS_NEER <- cbind(t(kpss_NEER@teststat), kpss_NEER@cval)
diffexpKPSS_NEER <- cbind(t(kpss_dNEER@teststat), kpss_dNEER@cval)
expKPSS_MMRATE <- cbind(t(kpss_MMRATE@teststat), kpss_MMRATE@cval)
diffexpKPSS_MMRATE <- cbind(t(kpss_dMMRATE@teststat), kpss_dMMRATE@cval)
expKPSS_PROFITMRG <- cbind(t(kpss_PROFITMRG@teststat), kpss_PROFITMRG@cval)
diffexpKPSS_PROFITMRG <- cbind(t(kpss_dPROFITMRG@teststat), kpss_dPROFITMRG@cval)
expKPSS_CORE <- cbind(t(kpss_CORE@teststat), kpss_CORE@cval)
diffexpKPSS_CORE <- cbind(t(kpss_dCORE@teststat), kpss_dCORE@cval)

kpss_matrix<-rbind(
  expKPSS_NEER,  
  diffexpKPSS_NEER, 
  expKPSS_MMRATE, 
  diffexpKPSS_MMRATE, 
  expKPSS_PROFITMRG,  
  diffexpKPSS_PROFITMRG, 
  expKPSS_CORE, 
  diffexpKPSS_CORE)  

kpss_matrix.df<-as.data.frame(kpss_matrix)
round_df(kpss_matrix, 3)
kpss_matrix.df <- cbind(Variable = rownames(kpss_matrix.df), kpss_matrix.df)
xtable(kpss_matrix, caption = "KPSS tests for Estonian data", centering="centering", label = "NULL", digits = 3)





setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/enterprise profitability")
eeugap_d<-read_excel("profmar.xls")
str(eeugap_d)
eeugap_d

eeugap_d.ts<-ts(eeugap_d, start =c(1996,1), frequency = 4)
eeugap_d.ts
diff(log(system1_balanced.ts[, "core.ts"]),4), type = "drift", lags = 2)
system1_balanced.ts
dlneer<-diff(log(system1_balanced.ts[,"neerEE.ts"]),4)
dlprofitmrg<-diff(log(system1_balanced.ts[,"profitmrg.ts"]),4)
dlcore<-diff(log(system1_balanced.ts[,"core.ts"]),4)
eemmrate<-system1_balanced.ts[,"mmrateEE.ts"]

EPeeugap.ts<-eeugap_d.ts[,c(2:6)] 
EPeeugap.ts
eeugap<-eeugap.ts[,1]

# rm(SVAR_matrix, SVAR_matrix_unadj)
SVAR_matrix_unadj<-cbind(eeugap, dlneer, eemmrate, dlprofitmrg, dlcore)
SVAR_matrix_unadj
SVAR_matrix.ts<-na.omit(SVAR_matrix_unadj)
plot(SVAR_matrix.ts)
plot(EPeeugap.ts)

par(mfrow=2:1)
plot(SVAR_matrix.ts)
plot(EPeeugap.ts)

var.1c<-VAR(EPeeugap.ts, p = 1 , type = "const")
summary(var.1c)
var.2c<-VAR(EPeeugap.ts, p = 2 , type = "const")
summary(var.2c)

VARselect(EPeeugap.ts, lag.max = 2, type = "const")
VARselect(EPeeugap.ts, lag.max = 2, type = "none")
VARselect(EPeeugap.ts, lag.max = 2, type = "both")

vartest<-VAR(EPeeugap.ts)



# Construct various A and B matrices
amat<-diag(5)
diag(amat)<-NA
amat1<-amat 
  amat1[2, 1] <- NA
  amat1[3, 1] <- NA
  amat1[4, 1] <- NA
  amat1[5, 1] <- NA
amat2<-amat1
  amat2[3, 2] <- NA
  amat2[4, 2] <- NA
  amat2[5, 2] <- NA
  amat2[4, 3] <- NA
  amat2[5, 3] <- NA
  amat2[5, 4] <- NA
  amat2  

bmat<-diag(5)
diag(bmat)<-NA  
bmat1<-bmat
bmat1[2, 1] <- NA
bmat1[3, 1] <- NA
bmat1[4, 1] <- NA
bmat1[5, 1] <- NA
bmat2<-bmat1
bmat2[3, 2] <- NA
bmat2[4, 2] <- NA
bmat2[5, 2] <- NA
bmat2[4, 3] <- NA
bmat2[5, 3] <- NA
bmat2[5, 4] <- NA
bmat2  

# Estimates an SVAR (‘A-model’) by using a scoring algorithm
svar1c_amod<-SVAR(x = var.1c, estmethod = "scoring", Amat = amat2, Bmat = NULL,lrtest = TRUE, max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
svar2c_amod<-SVAR(x = var.2c, estmethod = "scoring", Amat = amat2, Bmat = NULL,lrtest = TRUE, max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
svar2c_bmod<-SVAR(x = var.2c, estmethod = "scoring", Amat = NULL, Bmat = amat2,lrtest = TRUE, max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 

svar2test<-SVAR(x = vartest, estmethod = "scoring", Amat = amat2, Bmat = NULL,lrtest = TRUE, max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 

set.seed(123)
irf1c<-irf(svar1c_amod, boot = T, n.ahead = 20, ci = 0.95, runs = 3000)
set.seed(123)
irf2c<-irf(svar2c_amod, boot = T, n.ahead = 20, ci = 0.95, runs = 3000)
set.seed(123)
irftest<-irf(svar2test, boot = T, n.ahead = 20, ci = 0.95, runs = 3000)

irf2c_bmod<-irf(svar2c_bmod, boot = T, n.ahead = 20, ci = 0.95, runs = 3000)

irf2c_eeugap<-irf(svar2c_bmod, impulse = "eeugap", response = c("eeugap", "dlneer", "eemmrate", "dlprofitmrg", "dlcore"), boot = T, n.ahead = 20, ci = 0.95, runs = 3000)
irf2c_dlneer<-irf(svar2c_bmod, impulse = "dlneer", response = c("eeugap", "dlneer", "eemmrate", "dlprofitmrg", "dlcore"), boot = T, n.ahead = 20, ci = 0.95, runs = 3000)
irf2c_eemmrate<-irf(svar2c_bmod, impulse = "eemmrate", response = c("eeugap", "dlneer", "eemmrate", "dlprofitmrg", "dlcore"), boot = T, n.ahead = 20, ci = 0.95, runs = 3000)
irf2c_dlprofitmrg<-irf(svar2c_bmod, impulse = "dlprofitmrg", response = c("eeugap", "dlneer", "eemmrate", "dlprofitmrg", "dlcore"), boot = T, n.ahead = 20, ci = 0.95, runs = 3000)
irf2c_dlcore<-irf(svar2c_bmod, impulse = "dlcore", response = c("eeugap", "dlneer", "eemmrate", "dlprofitmrg", "dlcore"), boot = T, n.ahead = 20, ci = 0.95, runs = 3000)

# IRF analysis on device screen
plot(irf1c_amod, plot.type="multiple", nc = 2, ask = FALSE)
plot(irf2c_amod, plot.type="multiple", nc = 1, ask = FALSE)
plot(irftest, plot.type="multiple", nc = 2, ask = FALSE)
plot(irf2c_bmod, plot.type="multiple", nc = 2, ask = FALSE)

# PNG output (not for GNU Latex)
png('Filename%02c_bmood.png', width=6, height=6, units='in', res=300)
plot(irf2c_bmod, plot.type="multiple", nc = 1, ask = FALSE)                  # NC = number of columns, ask = Hit <Return> in the dev-environment 
dev.off()

png('Filename%02c_amood.png', width=6, height=6, units='in', res=300)
plot(irf2c_amod, plot.type="multiple", nc = 1, ask = FALSE)                  # NC = number of columns, ask = Hit <Return> in the dev-environment 
dev.off()


plot(irf2c_eeugap, plot.type="multiple", nc = 1)
## the TikZ environment can not produce seperate .tex files (on contrast to the .png output). Therefore, we need to call the plot tikz function individually for the shocks
# IRF TikZ Latex Output
tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/IRS_eeugap.tex",  
     standAlone = F, sanitize =T, width = 4, height = 5.5)
plot(irf2c_eeugap, plot.type="multiple", nc = 1)                  
dev.off()


# IRF TikZ Latex Output
tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/IRS_dlneer.tex",
     standAlone = F, sanitize =T, width = 4, height = 5.5)
plot(irf2c_dlneer, plot.type="multiple", nc = 1, ask = FALSE)                  
dev.off()

# IRF TikZ Latex Output
tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/IRS_eemmrate.tex",
     standAlone = F, sanitize =T, width = 4, height = 5.5)
plot(irf2c_eemmrate, plot.type="multiple", nc = 1, ask = FALSE)                  
dev.off()


# IRF TikZ Latex Output
tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/IRS_dlprofitmrg.tex",
     standAlone = F, sanitize =T, width = 4, height = 5.5)
plot(irf2c_dlprofitmrg, plot.type="multiple", nc = 1, ask = FALSE)                  
dev.off()


# IRF TikZ Latex Output
tikz("/Users/nicolasreigl/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/term project_enterprise profitability/IRS_dlcore.tex",
     standAlone = F, sanitize =T, width = 4, height = 5.5)
plot(irf2c_dlcore, plot.type="multiple", nc = 1, ask = FALSE)                 
dev.off()







################################################################################################################################
sessionInfo()
toLatex(sessionInfo())
################################################################################################################################
################################################################################################################################
