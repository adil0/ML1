#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# These are the R commands for Part 2 of the lectures notes for 46-926.
# Chad Schafer, Last Updated 1/8/17


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# The Fama-French Example
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


# Get my function for reading in the Fama-French data

source("http://www.stat.cmu.edu/~cschafer/MSCF/getFamaFrench.txt")


# Load quantmod

library(quantmod)


# Get PNC data

PNC = getSymbols("PNC", from="2016-1-1", to="2016-6-30", auto.assign=F)


# Get the Fama-French info from the same period

ffhold = getFamaFrench(from = "2016-1-1", to="2016-6-30")


# Find the excess returns

ffhold$PNCexret = 100*dailyReturn(PNC) - ffhold$RF


# Fit the three factor model

ff3modPNC = lm(PNCexret ~ Mkt.RF + SMB + HML, data=ffhold)


# Diagnostic Plots

postscript(file="PNCdiagnostic.eps",width=8,height=8,horiz=F)
par(mfrow=c(2,2))
plot(as.numeric(ff3modPNC$fit),as.numeric(ff3modPNC$resid),
   pch=16,xlab="Fitted Values", 
   ylab="Residuals",cex.axis=1.3,cex.lab=1.3)

qqnorm(as.numeric(ff3modPNC$resid),cex.axis=1.3,cex.lab=1.3,pch=16,main="")
qqline(as.numeric(ff3modPNC$resid))

plot(ff3modPNC$resid, xlab="Time",ylab="Residuals",cex.axis=1.3,cex.lab=1.3,
   pch=16,main="")
dev.off()


# Cook's Distance

cookd = as.numeric(cooks.distance(ff3modPNC))
sort(pf(cookd,4,121),decreasing=TRUE)[1:5]


# Robust Regression

library(MASS)

holdrlm = rlm(PNCexret ~ Mkt.RF + SMB + HML, data=ffhold)

holdrlm = rlm(PNCexret ~ Mkt.RF + SMB + HML, k=0.5, data=ffhold)


# Make a prediction

predict.lm(ff3modPNC, newdata=data.frame(Mkt.RF=0.01, SMB=0.1, HML=0.3), 
    interval="prediction")



#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# The Forward Rate Function Example
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

forratedat = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/forratedat.txt",
   header=T)

attach(forratedat)

# Raw plot of the data

postscript(file="stepwise1.eps",width=6,height=5,horiz=F)
plot(maturity,emp,pch=16,cex=1.0,cex.lab=1.3,xlab="maturity",
   ylab="empirical forward rate",cex.axis=1.3)
dev.off()


# Scale the maturity so that it has mean 0 and SD 1

maturity = scale(maturity)


# Consider the full model

fullmod = lm(emp ~ maturity + I(maturity^2) + I(maturity^3) + 
    I(maturity^4) + I(maturity^5) + I(maturity^6) + I(maturity^7) + 
    I(maturity^8) + I(maturity^9))


# Using bestglm with AIC

library(bestglm)

allpreds = cbind(maturity, maturity^2, maturity^3, maturity^4, maturity^5,
   maturity^6, maturity^7, maturity^8, maturity^9)

Xyframe = data.frame(cbind(allpreds,emp))

bestmod = bestglm(Xyframe, IC="AIC")


# Stepwise procedure

finalmod = step(fullmod, direction="both")


# Find the leverages

levs = hatvalues(fullmod)


# Calculate PRESS

PRESS = sum((fullmod$resid/(1-levs))^2)


# Using bestglm with LOOCV

bestmod2 = bestglm(Xyframe, IC="LOOCV")

