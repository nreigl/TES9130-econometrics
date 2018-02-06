# install necessary packages
PACKAGES<-c("ape", 
            "broom", 
            "brew", 
            "dplyr", 
            "vars", 
            "BMR", 
            "tidyr",
            "reshape2",
            "ggplot2", 
            "MSBVAR")
PACKAGES
class(PACKAGES)
inst<-match(PACKAGES, .packages(all=TRUE))
need<-which(is.na(inst))
if (length(need)>0) install.packages(PACKAGES[need])
# load required packages
lapply(PACKAGES, require, character.only=T)


# Vignette build
data(BMRVARData)
ls()
head(USMacroData)
plot.ts(USMacroData)
plot(USMacroData$INFLATION, type ="l")
plot(USMacroData$UNRATE, type ="l")
plot(USMacroData$FEDFUNDS, type ="l")
USMacroData[,2:4]

# Minnesota prior
prior<-c(0.9,0.95,0.95)
testbvarm <- BVARM(USMacroData[,2:4],prior,p=4,constant=T,irf.periods=20,
                   keep=10000,burnin=5000,VType=1,
                   HP1=0.5,HP2=0.5,HP3=10)
?BVARM
testbvarm
plot(testbvarm,save=F)

IRF(testbvarm,save=F)

str(testbvarm)
?IRF
IRF
forecast(testbvarm,shocks=TRUE,backdata=10,save=FALSE)

# Wishart prior
testbvarw <- BVARW(USMacroData[,2:4],cores=1,c(0.9,0.95,0.95),p=4,constant=T,
                   irf.periods=20,keep=10000,burnin=5000,
                   XiBeta=4,XiSigma=1,gamma=4)
plot(testbvarw,save=F)
IRF(testbvarw,save=F)
forecast(testbvarw,shocks=TRUE,backdata=10,save=FALSE)


?gacf
gacf(USMacroData[,2:4],lags=12,ci=0.95,plot=T,barcolor="purple",
     names=T,save=T,height=6,width=12)



# SZ, B-SVAR model for the Levant data
data(IsraelPalestineConflict)
rf.var <- reduced.form.var(IsraelPalestineConflict, p=6)
plot(irf(rf.var, nsteps = 12))


ident <- diag(3)
ident

USMacroData[,-1]
USMacroData.ts<-ts(USMacroData[,-1], start = c(1954,2), frequency =4 )
USMacroData.ts

?szbsvar
varobj <- szbsvar(Y=USMacroData.ts, p=6, z = NULL,
                  lambda=0.6, lambda1=0.1, lambda3=2, lambda4=0.25,
                  lambda5=0, mu5=0, mu6=0, ident, qm = 4)

A0.posterior <- gibbs.A0(varobj, N1=1000, N2=1000)
varobj

# Note you need to explcitly reference the sampled A0.posterior object
# in the following call for R to find it in the namespace!

impulse.sample <- mc.irf(varobj, nsteps=12, A0.posterior=A0.posterior)
impulse.sample

plot(impulse.sample)

# standard structral VAR
data(Canada)
plot.ts(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
amat <- diag(4)
amat
diag(amat) <- NA
amat[2, 1] <- NA
amat[4, 1] <- NA

## Estimation method scoring
SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL,
     max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
## Estimation method direct
SVAR(x = var.2c, estmethod = "direct", Amat = amat, Bmat = NULL,
     hessian = TRUE, method="BFGS")


## For SVAR
amat <- diag(4)
diag(amat) <- NA
svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
irf(svar.a, impulse = "e", response = c("prod", "rw", "U"), boot =FALSE)

plot(irf(svar.a))

data(IsraelPalestineConflict)
varnames <- colnames(IsraelPalestineConflict)

fit.BVAR <- szbvar(Y=IsraelPalestineConflict, p=6, z=NULL,
                   lambda0=0.6, lambda1=0.1,
                   lambda3=2, lambda4=0.25, lambda5=0, mu5=0,
                   mu6=0, nu=3, qm=4,
                   prior=0, posterior.fit=FALSE)

# Draw from the posterior pdf of the impulse responses.
posterior.impulses <- mc.irf(fit.BVAR, nsteps=10, draws=5000)

# Plot the responses
plot(posterior.impulses, method=c("Sims-Zha2"), component=1,
     probs=c(0.16,0.84), varnames=varnames)

# Example 2
ident <- diag(2)
varobj <- szbsvar(Y=IsraelPalestineConflict, p=6, z = NULL,
                  lambda=0.6, lambda1=0.1, lambda3=2, lambda4=0.25,
                  lambda5=0, mu5=0, mu6=0, ident, qm = 4)

A0.posterior <- gibbs.A0(varobj, N1=1000, N2=1000)

# Note you need to explcitly reference the sampled A0.posterior object
# in the following call for R to find it in the namespace!

impulse.sample <- mc.irf(varobj, nsteps=12, A0.posterior=A0.posterior)

plot(impulse.sample, varnames=colnames(IsraelPalestineConflict),
     probs=c(0.16,0.84))