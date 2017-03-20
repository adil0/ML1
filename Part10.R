
#-------------------------------------------------------------------
# These are the R commands for Clustering


stockmat = read.table(
  "http://www.stat.cmu.edu/~cschafer/MSCF/stockdata.txt",header=T)


# Determine the full name for each stock

firmdata = read.csv(
  "http://www.stat.cmu.edu/~cschafer/MSCF/AllFirms.csv",header=F)

fullname = rep("a",ncol(stockmat))

for(i in 1:ncol(stockmat))
{
  holdout = firmdata[which(firmdata[,1]==names(stockmat)[i]),2]
  if(length(holdout)==0)
  {
     fullname[i] = as.character(names(stockmat)[i])  
  } else
  {
     fullname[i] = as.character(holdout)
   }
}



#-------------------------------------------------------------------
# Run K-means on the data

set.seed(0)
kmout = kmeans(t(stockmat),centers=6,nstart=10)


# Plot the results

for(k in 1:6)
{
  if(k %% 3 == 1)
  {
    postscript(file=paste("kmeans",floor(k/3+1),".eps",sep=""),
               horiz=F,width=6,height=9)
    par(mfrow=c(3,1))
  }
  plot(1:42,kmout$centers[k,],xlim=c(1,42),
       ylim=c(min(stockmat),max(stockmat)),type="l",col=2,lwd=2,
       xlab="Day",ylab="Adjusted Price (Scaled)",cex.lab=1.3,
       cex.axis=1.3,lty=2)
  
  for(i in 1:length(SymbolList$V1))
  {
    if(kmout$cluster[i]==k)
    {
      points(1:42,stockmat[,i],type="l",col=rgb(0.3,0.3,0.3))
    }
  }
  
  points(1:42,kmout$centers[k,],type="l",col=2,lwd=2,lty=2)
  if(k %% 3 == 0) 
  {
    dev.off()
  }
}




#-------------------------------------------------------------------
# Hierarchical Clustering

# Simple Dendrogram Example

set.seed(0)
x = rnorm(10, c(0,0,0,0,0,5,5,5,7,7), sd = 0.5)

postscript(file="simpdend.eps",width=6,height=6,horiz=F)
plot(hclust(dist(x)),xlab="",sub="")
dev.off()


# Cluster our stock data

hcout = hclust(dist(t(stockmat)), method="complete")

postscript(file="stockdend.eps",width=8,height=6,horiz=T)
plot(hcout, labels=fullname, cex=0.45, sub="", xlab="")
dev.off()


