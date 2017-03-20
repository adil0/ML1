
#-----------------------------------------------------------------------------
# R Commands for Part 8, Regression Trees


library(tree)
library(mgcv)

source("http://www.stat.cmu.edu/~cschafer/MSCF/cv.tree.full.txt")


# Read in the data file

alldat = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/OptionsData.txt",
     header=T)

alldat$transresp = (alldat$last)^(1/4)


#-----------------------------------------------------------------------------
# Simple Tree, mostly for plotting purposes

treesimp = tree(transresp ~ strike + curprice, data=alldat)

treesimppruned = prune.tree(treesimp,k=5)

postscript(file="treesimp.eps",horiz=F,height=7,width=6)
plot(treesimppruned)
text(treesimppruned,cex=0.75)
dev.off()

postscript(file="partitionsimp.eps",horiz=F,height=7,width=7)
partition.tree(treesimppruned,cex=0.75)
dev.off()


# Make the progression of trees

treesimp1 = tree(transresp ~ strike + curprice, data=alldat,
   mindev=0.1,minsize=2)
treesimp2 = tree(transresp ~ strike + curprice, data=alldat,
   mindev=0.04,minsize=2)
treesimp3 = tree(transresp ~ strike + curprice, data=alldat,
   mindev=0.03,minsize=2)
treesimp4 = tree(transresp ~ strike + curprice, data=alldat,
   mindev=0,minsize=1)


postscript(file="growingtree.eps",horiz=F,height=6,width=6)
layout(t(matrix(c(1,2,3,4),nrow=2)))
par(mar=c(2,2,2,2))
plot(treesimp1)
text(treesimp1)
plot(treesimp2)
text(treesimp2)
plot(treesimp3)
text(treesimp3)
plot(treesimp4)
dev.off()




#-----------------------------------------------------------------------------
# For the simple R code example

fulltree = tree(transresp ~ strike + curprice, data=alldat, 
   mindev=0, minsize=2)

prunedtree = prune.tree(fulltree, k=5)

prunedtree2 = prune.tree(fulltree, best=10)

postscript(file="prunedtree.eps",horiz=F,height=6,width=6)
plot(prunedtree)
text(prunedtree, cex=0.75)
dev.off()


cvout = cv.tree.full(fulltree)

postscript(file="cvplot.eps",horiz=F,height=5,width=6)
plot(cvout)
dev.off()

optalpha = cvout$k[which.min(cvout$dev)]
print(cvout$size[which.min(cvout$dev)])

opttree = prune.tree(fulltree, k=optalpha)

postscript(file="opttree.eps",horiz=F,height=6,width=6)
plot(opttree)
text(opttree, cex=0.75)
dev.off()



#-----------------------------------------------------------------------------
# Model 7: Regression Tree


set.seed(0)
model7fulltree = tree(transresp ~ timetoexpiry + strike + curprice + histvol, 
     data=alldat, mindev=0, minsize=2)

cvmodel7 = cv.tree.full(model7fulltree)

model7optalpha = cvmodel7$k[which.min(cvmodel7$dev)]
model7opttree = prune.tree(model7fulltree, k=model7optalpha)

print(summary(model7opttree))

postscript(file="model7respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model7opttree),alldat$transresp,
   pch=16,cex=0.7,xlab="Predicted Value",ylab="Actual Response",
   cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()

postscript(file="model7resvsfit.eps",horiz=F,width=7,height=6)
plot(predict(model7opttree), residuals(model7opttree),
   xlab="Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model7cvout.eps",horiz=F,width=7,height=6)
plot(cvmodel7)
dev.off()

postscript(file="model7opttree.eps",horiz=F,height=6,width=6)
plot(model7opttree)
text(model7opttree, cex=0.5)
dev.off()



#-----------------------------------------------------------------------------
# Model 8: Fit a simpler model

model8opttree = prune.tree(model7fulltree, best=175)

print(summary(model8opttree))

postscript(file="model8respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model8opttree),alldat$transresp,
   pch=16,cex=0.7,xlab="Predicted Value",ylab="Actual Response",
   cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()

postscript(file="model8resvsfit.eps",horiz=F,width=7,height=6)
plot(predict(model8opttree), residuals(model8opttree),
   xlab="Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model8opttree.eps",horiz=F,height=6,width=6)
plot(model8opttree)
text(model8opttree, cex=0.5)
dev.off()




#----------------------------------------------------------------------
#----------------------------------------------------------------------
# THE CLASSIFICATION TREE EXAMPLE



# Read in the data

ratedata = read.table(
   "http://www.stat.cmu.edu/~cschafer/MSCF/RateData.txt",header=T)


# The full tree

fulltree = tree(factor(Rating) ~ factor(ext.source)+factor(EDF.source)+
   Profit+Capital+Excess.Net.Capital+Liab.Capital+Leverage+Liquidity.1+
   Liquidity+factor(Descr), data=ratedata, mindev=0, minsize=2)


postscript(file="ratefulltree.eps",width=7,height=7,horiz=F)
plot(fulltree)
text(fulltree)
dev.off()

# Run cross-validation procedure

set.seed(0)
cvout = cv.tree.full(fulltree)

postscript(file="ratecv.eps",width=7,height=7,horiz=F)
plot(cvout)
dev.off()

# Choose the optimal model

alphaopt = cvout$k[which.min(cvout$dev)]

opttree = prune.tree(fulltree, k=alphaopt)

postscript(file="rateopttree.eps",width=7,height=7,horiz=F)
plot(opttree)
text(opttree)
dev.off()



#----------------------------------------------------------------------
#----------------------------------------------------------------------
# THE RANDOM FORESTS EXAMPLE

# Read in the data file

alldat = read.table(
   "http://www.stat.cmu.edu/~cschafer/MSCF/OptionsData.txt",header=T)

alldat$transresp = (alldat$last)^(1/4)


# Fit Random Forest

library(randomForest)

set.seed(0)
model9 = randomForest(transresp ~ timetoexpiry + strike + curprice + 
     histvol, data=alldat, importance=TRUE)

postscript(file="model9plot.eps",horiz=F,height=5,width=6)
plot(model9)
dev.off()

residsmodel9 = alldat$transresp - predict(model9)

postscript(file="model9resvsfit.eps",horiz=F,width=7,height=6)
plot(predict(model9), residsmodel9, xlab="Fitted Values", 
   ylab="Residuals", cex.axis=1.3, cex.lab=1.3, pch=16, cex=0.7)
dev.off()

postscript(file="model9respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model9), alldat$transresp, pch=16, cex=0.7,
   xlab="Predicted Value", ylab="Actual Response", cex.axis=1.3,
   cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()



# Optimize for m

xmat = alldat[,c(1,2,8,9)]

print(tuneRF(xmat,alldat$transresp))

model10 = randomForest(formula = transresp ~ timetoexpiry + 
    strike + curprice + histvol, data = alldat, importance = TRUE,
    mtry=4)

residsmodel10 = alldat$transresp - predict(model10)

postscript(file="model10resvsfit.eps",horiz=F,width=7,height=6)
plot(predict(model10), residsmodel10, xlab="Fitted Values", 
   ylab="Residuals",cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
dev.off()

postscript(file="model10respvspreds.eps", horiz=F,width=7,height=6)
plot(predict(model10),alldat$transresp,pch=16,cex=0.7,
   xlab="Predicted Value",ylab="Actual Response", cex.axis=1.3,
   cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()



#----------------------------------------------------------------------
#----------------------------------------------------------------------
# BOOSTING REGRESSION TREE EXAMPLE


# This is necessary to avoid the bug in the code

alldatsub = alldat[,c(1,2,8,9,11)]

library(gbm)

set.seed(0)
model11 = gbm(transresp ~ timetoexpiry +strike + curprice + histvol, 
              data = alldatsub, bag.fraction=0.5, distribution="gaussian",
              n.trees=200000, interaction.depth=3, shrinkage = 0.001,
              cv.folds=3)

# Find the best model via cross-validation.

postscript(file="cvmodel11.eps",width=8,height=6,horiz=F)
gbm.perf(model11, method="cv")
dev.off()

optnumtrees = 50000

# Get the fitted values for this model

model11fits = predict(model11,n.trees=optnumtrees)

# Find RSS for this model

sum((model11fits-alldat$transresp)^2)

# Diagnostic Plots

postscript(file="model11resvsfit.eps",horiz=F,width=7,height=6)
plot(model11fits, alldat$transresp-model11fits, xlab="Fitted Values", 
     ylab="Residuals", cex.axis=1.3, cex.lab=1.3, pch=16, cex=0.7)
dev.off()

postscript(file="model11respvspreds.eps", horiz=F,width=7,height=6)
plot(model11fits, alldat$transresp, pch=16, cex=0.7,
     xlab="Predicted Value", ylab="Actual Response", cex.axis=1.3,
     cex.lab=1.3)
abline(0,1,lwd=2,col=4)
dev.off()

