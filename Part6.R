#-----------------------------------------------------------------------------
# Read in the data file

alldat = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/OptionsData.txt",
   header=T)

# Make plot of predictors, motivate log transform

postscript(file="optionshists.eps",horiz=F,width=6,height=5)
layout(matrix(1:4,nrow=2))
par(mar=c(4,4,2,2))
hist(alldat$timetoexpiry, xlab="Time to Expiry (days)", ylab="",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
hist(alldat$curprice, xlab="Current Equity Price", ylab="density",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
hist(alldat$histvol, xlab="Historical Volatility", ylab="density",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
hist(alldat$strike, xlab="Strike Price", ylab="density",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
dev.off()

postscript(file="optionshistslog.eps",horiz=F,width=6,height=5)
layout(matrix(1:4,nrow=2))
par(mar=c(4,4,2,2))
hist(log(alldat$timetoexpiry), xlab="Log Time to Expiry (days)", ylab="",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
hist(log(alldat$curprice), xlab="Log Current Equity Price", ylab="density",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
hist(log(alldat$histvol), xlab="Log Historical Volatility", ylab="density",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
hist(log(alldat$strike), xlab="Log Strike Price", ylab="density",
  cex.axis=1.3,cex.lab=1.3, prob=T, main="")
dev.off()




#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# The sequence of models

# Model 1: Linear Model without interaction or transformation of response
model1 = lm(last ~ log(timetoexpiry) + log(strike) + log(curprice) + 
    log(histvol), data=alldat)

postscript(file="model1resvsfit.eps",horiz=F,width=7,height=6)
plot(model1$fit, model1$residuals, xlab="Fitted Values", ylab="Residuals", 
   cex.axis=1.3, cex.lab=1.3, pch=16, cex=0.7)
dev.off()

print(summary(model1))



#-----------------------------------------------------------------------------
# Model 2: Transform the response

# Box-Cox Procedure

library(car)
postscript(file="model2boxcox.eps",horiz=F,width=7,height=6)
boxCox(last ~ log(timetoexpiry) + log(strike) + log(curprice) + log(histvol), 
   data=alldat)
dev.off()

alldat$transresp = alldat$last^(1/4)

model2 = lm(transresp ~ log(timetoexpiry) + log(strike) + log(curprice) + 
    log(histvol), data=alldat)


print(summary(model2))


postscript(file="model2resvsfit.eps",horiz=F,width=7,height=6)
plot(model2$fit, model2$residuals, xlab="Fitted Values", ylab="Residuals",
   cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model2respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model2),alldat$transresp,pch=16,cex=0.7,xlab="Predicted Value",
   ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()


#-----------------------------------------------------------------------------
# Model 3: Additive Model

library(mgcv)

model3 = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)), data=alldat)

postscript(file="model3resvsfit.eps",horiz=F,width=7,height=6)
plot(model3$fit, model3$residuals, xlab="Fitted Values", ylab="Residuals",
   cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model3respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model3),alldat$transresp,pch=16,cex=0.7,xlab="Predicted Value",
   ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()

postscript(file="model3fits.eps", horiz=F,width=7,height=8)
plot(model3, pages=1,scale=0,scheme=1)
dev.off()

print(summary(model3))


#-----------------------------------------------------------------------------
# Model 4: Additive Model with interaction

model4a = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)) + s(log(timetoexpiry),log(strike)),
   data=alldat)

model4b = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)) + s(log(timetoexpiry),log(curprice)),
   data=alldat)

model4c = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)) + s(log(timetoexpiry),log(histvol)),
   data=alldat)

model4d = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)) + s(log(strike),log(curprice)),
   data=alldat)

model4e = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)) + s(log(strike),log(histvol)),
   data=alldat)

model4f = gam(transresp ~ s(log(timetoexpiry)) + s(log(strike)) + 
   s(log(curprice)) + s(log(histvol)) + s(log(curprice),log(histvol)),
   data=alldat)

# Choose Model 4d, as it has significantly larger likelihood than the
# others, and its AIC is much smaller than that of Model 3.

model4 = model4d

print(summary(model4))

postscript(file="model4resvsfit.eps",horiz=F,width=7,height=6)
plot(model4$fit, model4$residuals,
   xlab="Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3,
   pch=16,cex=0.7)
dev.off()

postscript(file="model4respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model4),alldat$transresp,pch=16,cex=0.7,xlab="Predicted Value",
   ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()

postscript(file="model4fits.eps", horiz=F,width=6,height=8)
plot(model4, pages=1,scheme=1,scale=0)
dev.off()


#-----------------------------------------------------------------------------
# Model 5, PPR

# The cross-validation procedure

source("http://www.stat.cmu.edu/~cschafer/MSCF/CVforppr.R")

modelformula = transresp ~ log(timetoexpiry) + log(strike) + log(curprice) + 
   log(histvol)

pprCV = matrix(0,nrow=10,ncol=4)

for(j in 1:ncol(pprCV))
{
   set.seed(j)
   for(i in 1:nrow(pprCV))
   {
      pprCV[i,j] = CVforppr(modelformula, nterms=i, numfolds=10, data=alldat, 
         sm.method="gcvspline")
   }
}


# Plot of CV Results

postscript(file="cvpprresults.eps",width=7, height=6,horiz=F)
plot(1:nrow(pprCV),apply(pprCV,1,mean),type="b",pch=16,col=2,cex.axis=1.3,
   cex.lab=1.3,xlab="Number of Ridge Functions (M)",
   ylab="Squared Error (via Cross-Validation)")

for(j in 1:ncol(pprCV))
{
   points(1:nrow(pprCV),pprCV[,j],pch=16)
}
dev.off()


model5 = ppr(transresp ~ log(timetoexpiry) + log(strike) + log(curprice) + 
      log(histvol), nterms = 4, data=alldat, sm.method="gcvspline")


# Plot the ridge functions

postscript(file="ridgefuncs.eps",width=7, height=6,horiz=F)
par(mfrow=c(2,2))
plot(model5)
dev.off()

postscript(file="model5resvsfit.eps",horiz=F,width=7,height=6)
plot(model5$fit, model5$residuals, xlab="Fitted Values", ylab="Residuals",
   cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model5respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model5),alldat$transresp,pch=16,cex=0.7,xlab="Predicted Value",
   ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()


#-----------------------------------------------------------------------------
# Model 6, Neural Network

library(nnet)

model6 = nnet(transresp ~ scale(log(timetoexpiry)) + scale(log(strike)) + 
   scale(log(curprice)) + scale(log(histvol)), data=alldat, size=10, 
   linout=TRUE, decay = 0.001, maxit=1000)

# Cross-Validation

source("http://www.stat.cmu.edu/~cschafer/MSCF/CVfornnet.R")

modelformula = transresp ~ scale(log(timetoexpiry)) + scale(log(strike)) + 
   scale(log(curprice)) + scale(log(histvol))

nnetCV = array(0,dim=c(5,5,4))
decaylist = c(0,0.0001,0.001, 0.01, 0.02)
Mlist = c(5,10,15,25,50)


# WARNING: This takes about a half hour to run

for(k in 1:dim(nnetCV)[[3]])
{
   set.seed(k)
   for(i in 1:length(decaylist))
   {
      for(j in 1:length(Mlist))
      {
         nnetCV[i,j,k] = CVfornnet(modelformula, size=Mlist[j], 
            decay=decaylist[i], numfolds=10, data=alldat)
      }
   }
}

postscript(file="model6resvsfit.eps",horiz=F,width=7,height=6)
plot(model6$fit, model6$residuals, xlab="Fitted Values", ylab="Residuals",
   cex.axis=1.3, cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model6respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model6), alldat$transresp,pch=16,cex=0.7,xlab="Predicted Value",
   ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()
