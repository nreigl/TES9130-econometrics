# ts_121115

### Load and require snippet
# install necessary packages
PACKAGES<-c("dplyr","vars","brew", "seasonal", "boot", "Hmisc")

inst<-match(PACKAGES, .packages(all=TRUE))
need<-which(is.na(inst))
if (length(need)>0) install.packages(PACKAGES[need])

# load required packages
lapply(PACKAGES, require, character.only=T)


# load dataset
CPI_VAR <- read.table("~/studium/TUT/econometric (phd) - TES9130/matlab/Y_CPI_Non.csv", header=FALSE)
CPI_VAR <- read.csv("~/studium/TUT/econometric (phd) - TES9130/matlab/Y_CPI_Non.csv", header=FALSE)

CPI_VAR.ts<-ts(CPI_VAR,  start = c(1970,1), freq = 12)

colnames(CPI_VAR.ts)<-c("Output","CPI", "CPI_exEn", "StockRe", "FedFund"  )
head(CPI_VAR.ts)


plot(CPI_VAR.ts, type = "l")

# variable names
# Y_CPI_NonEnergy_RealS_FF197001-200706
#### create a restriction matrix for the monetary policiy shock
# 


# run standard VAR descriptive statistics
# run impulse response
# 



VARselect(model, lag.max = 4, type = "both") # 

# VAR(1)
p1ct<-VAR(model, p = 1, type = "both")
p1ct
summary(p1ct, equation = "emp")


# VAR(2)
p2ct<-VAR(model, p = 2, type = "both")
p2ct
summary(p2ct, equation = "emp")
