# ts_111115

### Load and require snippet
# install necessary packages
PACKAGES<-c("dplyr","vars","brew")

inst<-match(PACKAGES, .packages(all=TRUE))
need<-which(is.na(inst))
if (length(need)>0) install.packages(PACKAGES[need])

# load required packages
lapply(PACKAGES, require, character.only=T)


# load dataset
CPI_1970_2007 <- read.table("~/studium/TUT/econometric (phd) - TES9130/matlab/CPI_1970_2007.csv", quote="\"", comment.char="")


# format as time series object
CPI<-ts(CPI_1970_2007, start = c(1970,1), freq = 12)

plot(decompose(CPI))
