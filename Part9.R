#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# These are the R commands for Support Vector Machines

# Load the library and the data

library(e1071)
library(randomForest)
library(tree)
source("http://www.stat.cmu.edu/~cschafer/MSCF/cv.tree.full.txt")

# Read in the data

oneday = read.table(
   "http://www.stat.cmu.edu/~cschafer/MSCF/cprod_data_20130103v.txt")

# Create the indicator for the direction of change. This is what we 
# are going to predict.

oneday$change = as.numeric((oneday$V1 - oneday$V3) > 0)


# Divide the data into training and test sets

set.seed(0)

trainrows = sample(1:nrow(oneday), replace=F, size=10000)

trainset = oneday[trainrows,8:35]
testset = oneday[-trainrows,8:35]


# WARNING: This next command takes a while

svmout = tune.svm(factor(change) ~ ., data=trainset, 
    cost=c(5,10,25,50))


# Make a plot of the results

postscript(file="svmplot.eps",width=8,height=7,horiz=F)
plot(svmout$best.model, trainset, V10~V9, cex.axis=1.3,cex.lab=1.3)
dev.off()



#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# Compare with other methods


# Logistic Regression

logregout = glm(factor(change) ~., data=trainset, family=binomial)
logregout = step(logregout)


# Regression Tree

fulltree = tree(factor(change) ~ ., data=trainset, mindev=0, minsize=2)
cvout = cv.tree.full(fulltree)
alphaopt = cvout$k[which.min(cvout$dev)]
classtreeout = prune.tree(fulltree, k=alphaopt)


# Random Forest.

rfout = randomForest(factor(change) ~ ., data=trainset)


# Tables showing the results

svmtab = table(testset$change, predict(svmout$best.model,testset))
logregtab = table(testset$change, as.numeric(predict(logregout,
   newdata=testset,type="response") > 0.5))
classtreetab = table(testset$change, predict(classtreeout,
   newdata=testset,type="class"))
rftab = table(testset$change, predict(rfout, newdata=testset))

