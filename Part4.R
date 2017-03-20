yielddata = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/YieldCurves2012.txt", header=T)

yieldcurves = yielddata[,2:12]

years = c(1/12,3/12,1/2,1,2,3,5,7,10,20,30)


# Find the VIFs

library(car)


simy = rnorm(nrow(yieldcurves))
holdglm = glm(simy ~ 
  yieldcurves[,1] + 
  yieldcurves[,2] + 
  yieldcurves[,3] + 
  yieldcurves[,4] + 
  yieldcurves[,5] + 
  yieldcurves[,6] + 
  yieldcurves[,7] + 
  yieldcurves[,8] + 
  yieldcurves[,9] + 
  yieldcurves[,10] + 
  yieldcurves[,11]
 )

print(vif(holdglm))

# Create a plot comparing some yield curves

touse = c(10,100,200)
coluse = c(1,2,4)
ltyuse = c(1,2,4)

postscript(file="exampleyields.eps",horiz=F,width=8,height=7)

plot(years, yieldcurves[touse[1],], lwd=2, type="b", xlab="Years", ylab="Rate",
   cex.axis=1.3,cex.lab=1.3,ylim=c(0,max(yieldcurves[touse,])),pch=16)

for(i in 2:length(touse))
{
   lines(years, yieldcurves[touse[i],], lwd=2, col=coluse[i], lty=ltyuse[i], pch=16, type="b")
}
legend(30,0,legend=yielddata$Date[touse],lty=ltyuse,col=coluse,lwd=2,xjust=1,yjust=0,cex=1.5,pch=16)

dev.off()

# Run the pca

pcaout = princomp(yieldcurves)


# Where is the center

postscript(file="thecenter.eps",width=8,height=7,horiz=F)
plot(years,pcaout$center,lwd=2,type="b",xlab="Years",ylab="Rate",col=2,
cex.axis=1.3,cex.lab=1.3,pch=16,ylim=c(0,max(yieldcurves[touse,])))
dev.off()


# Construct the plots that show the effect of varying the components

postscript(file="varyone.eps",width=8,height=7,horiz=F)
plot(years,pcaout$center,lwd=2,type="b",xlab="Years",ylab="Rate",col=2,
cex.axis=1.3,cex.lab=1.3,pch=16)

offlist = seq(-1,1,length=6)
for(off in offlist)
{
   if(off < 0)
   {
      lines(years,(pcaout$center+off*pcaout$loadings[,1]),lwd=1,lty=2,col=4)
   } else
   {
      lines(years,(pcaout$center+off*pcaout$loadings[,1]),lwd=1,lty=4,col=2)
   }
}
dev.off()

postscript(file="varytwo.eps",width=8,height=7,horiz=F)
plot(years,pcaout$center,lwd=2,type="b",xlab="Years",ylab="Rate",col=2,
cex.axis=1.3,cex.lab=1.3,pch=16)

for(off in offlist)
{
   if(off < 0)
   {
      lines(years,(pcaout$center+off*pcaout$loadings[,2]),lwd=1,lty=2,col=4)
   } else
   {
      lines(years,(pcaout$center+off*pcaout$loadings[,2]),lwd=1,lty=4,col=2)
   }
}
dev.off()


postscript(file="varythree.eps",width=8,height=7,horiz=F)
plot(years,pcaout$center,lwd=2,type="b",xlab="Years",ylab="Rate",col=2,
cex.axis=1.3,cex.lab=1.3,pch=16)

for(off in offlist)
{
   if(off < 0)
   {
      lines(years,(pcaout$center+off*pcaout$loadings[,3]),lwd=1,lty=2,col=4)
   } else
   {
      lines(years,(pcaout$center+off*pcaout$loadings[,3]),lwd=1,lty=4,col=2)
   }
}
dev.off()





#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# LASSO Example, replicating HDG with a portfolio of NYSE stocks

library(glmnet)

retmat = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/replicatingHDG.dat")
retmat = as.matrix(retmat)


# Get the data for the stock under consideration
# Note that the final column holds the response

glmnetout = glmnet(retmat[,-1962], retmat[,1962])

# Use cross-validation to choose optimal lambda. Note that this procedure is random.
set.seed(0)

cvglmout = cv.glmnet(retmat[,-1962], retmat[,1962])

postscript(file="mincv.eps",width=8,height=6,horiz=F)
plot(cvglmout,lwd=2)
abline(v=log(cvglmout$lambda.min),lty=3,lwd=2)
abline(v=log(cvglmout$lambda.1se),lty=3,lwd=2)

holdpos = which(cvglmout$glmnet.fit$lambda == cvglmout$lambda.1se)
lines(c(log(cvglmout$lambda.min),log(cvglmout$lambda.1se)),
      c(rep(cvglmout$cvm[holdpos],2)),lwd=3,col=4,lty=1)
dev.off()
