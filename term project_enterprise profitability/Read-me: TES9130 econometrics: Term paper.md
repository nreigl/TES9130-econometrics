Read-me: TES9130 econometrics: Term paper
=========================== 
# Read-me: TES9130 econometrics - Term paper



## Introduction
The project files consists out off:
 - one R.file
 - 2 excel/ csv files

The code is rather extensive and requires access to EUROSTAT (You have to be online to run the code!). First, please run the project environment setup. For the seasonal adjustment it is necessary to run the X-13-ARIMA-SEATS algorithm. On windows based systems you simply have to install the "seasonal" package. On UNIX based system you have to compile the binary executables by yourself. See the [CRAN](https://cran.r-project.org/web/packages/seasonal/README.html#installation) manual for further information. 


## R-file
One can run the full code from section 1 to section 5. Section 1 and 2 are mainly concerned with data scrapping from the EUROSTAT and OECD database and data manipulation. The main econometric part starts section 4. I have created excel and csv files that can be side-loaded in section 3 so one does not have to run section 1 and 2, or put differently: After running the environment set-up one can immediately continue with section 3 if necessary.

Please do not forget to change the working directory if you want to download the results!


