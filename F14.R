library(fANCOVA)
library(tidyverse)
library(car)

housedat = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/CalifHousing.txt", header=T, sep=",")

plot(housedat$MedianHouseValue, housedat$MedianIncome, pch=".")

# Question 1
loclin <- loess.as(housedat$MedianHouseValue, housedat$MedianIncome, criterion = "aicc")
summary(loclin)
plot(loclin$fitted, loclin$residuals, pch=".")


# Question 2
mlin <- lm(MedianIncome~MedianHouseValue+ HousingMedianAge + log(Households) + log(RoomsOverPopulation)+ 
           log(BedroomsOverPopulation) + log(PopulationOverHouseholds), data=housedat)
summary(mlin)

dat= data.frame(mlin$fitted.values,mlin$residuals)
names(dat) <- c("Fitted_Values","Residuals")
ggplot(dat, aes(x=Fitted_Values, y=Residuals)) +
  geom_point(shape=1)
qqnorm(dat$Residuals);qqline(dat$Residuals, col=2)
boxCox(lm(Ret_PlusOne~.,data = traindata), family="yjPower")



