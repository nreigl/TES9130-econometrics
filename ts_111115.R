# ts_111115

### Load and require snippet
# install necessary packages
PACKAGES<-c("dplyr","vars","brew", "seasonal")

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


# The installation of the seasonal can be tricky
# ## See: 
# Getting X-13
# 
# seasonal does not include the binary executables of X-13ARIMA-SEATS. They can be obtained precompiled from here (Windows: x13ashtmlall.zip). There are guides for building it from source for Ubuntu or Mac OS-X.
# 
# Download the file, unzip it and copy x13ashtml.exe (or x13ashtml, on Linux or OS-X) to any desired location on your file system.
# Telling R where to find X-13
# 
# Next, you need to tell seasonal where to find the binary executables of X-13ARIMA-SEATS, by setting the specific environmental variable X13_PATH. This may be done during your active session in R:
#   
#   Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")
# 
# Exchange YOUR_X13_DIRECTORY with the path to your installation of X-13ARIMA-SEATS. Note that the Windows path C:\something\somemore has to be entered UNIX-like C:/something/somemore or C:\\something\\somemore. You can always check your installation with:
#   
#   checkX13()
# 
# If it works, you may want to set the environmental variable permanently, by adding the Sys.setenv line to one of your .Rprofile files. The easiest is to use the one located in your home directory, which can be written directly from R:
#   
#   write('Sys.setenv(X13_PATH = "YOUR_X13_DIRECTORY")', 
#         file = "~/.Rprofile", append = TRUE)
# 
# If the file does not exist (by default), it will be created. Make sure that you get the quotes right: double quotes around your directory, single quotes around the whole Sys.setenv line, such that R understands your string. Check first that the the Sys.setenv line works correctly; once it is written you may have to edit .Rprofile manually. (Or add a second, overwriting line to it.) For other ways to set an environmental variable permanently in R, see ?Startup.

SA_CPI<-seas(CPI)
out(SA_CPI)
plot(SA_CPI)
