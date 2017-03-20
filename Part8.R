# Read in the data

bankdata = read.table(
        "http://www.stat.cmu.edu/~cschafer/MSCF/OlmedaData.txt")

# Name the variables

names(bankdata) = c('assets_tot','assetMcash_tot','assets_loans',
                 'reverse_loans','income_assets','income_equity',
                 'income_loans','cost_sales','cf_loans','default')

# Change the default variable into 0/1

bankdata$default = as.numeric(bankdata$default == "Failed")

# This function allows for direct reference to the named columns of
# the data frame bankdata. So, instead of writing bankdata$assets_tot,
# I can just write assets_tot

attach(bankdata)

# We need the package car for the Yeo-Johnson Transformation

library(car)

# Apply all of the transformations

bankdata$reverse_loans_trans = log(reverse_loans)

lambda = powerTransform(income_assets, family="yjPower")$roundlam
bankdata$income_assets_trans = yjPower(income_assets, lambda)

lambda = powerTransform(income_equity, family="yjPower")$roundlam
bankdata$income_equity_trans = yjPower(income_equity, lambda)

lambda = powerTransform(income_loans, family="yjPower")$roundlam
bankdata$income_loans_trans = yjPower(income_loans, lambda)

lambda = powerTransform(cf_loans, family="yjPower")$roundlam
bankdata$cf_loans_trans = yjPower(cf_loans, lambda)


# Fit the model

bankglm = glm(default ~ assets_tot + assetMcash_tot
 + assets_loans + reverse_loans_trans 
 + income_assets_trans + income_equity_trans 
 + income_loans_trans + cost_sales 
 + cf_loans_trans, family=binomial, data=bankdata)

finalmod = step(bankglm)


# Plot of response versus fitted probabilities

postscript(file="respvspreds.eps",width=8,height=6,horiz=F)
plot(fitted.values(finalmod),jitter(default,0.2),pch=16,
   xlab="Fitted Values",ylab="Observed Response (Jittered)",
   cex.axis=1.3,cex.lab=1.3)
dev.off()


# Hosmer-Lemeshow Test

library(ResourceSelection)
print(hoslem.test(default, finalmod$fit))


# Cooks Distance

cd = cooks.distance(finalmod)
postscript(file="cooksdist.eps",width=8,height=6,horiz=F)
plot(cd,xlab="Observation", ylab="Cook's Distance",cex.axis=1.3,cex.lab=1.3,pch=16)
dev.off()


# The stars plot

stars(bankdata[,c(1,3,11,12,14)], labels=1:66, col.lines=(bankdata$default+1))


# Example of making a prediction

predict(finalmod, type="response", se.fit=T)

