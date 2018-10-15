
AllData <- read.table("GradedHW1-All-Data.csv",header=T,sep=",",
                      stringsAsFactors = F,na.strings="")

AllData <- AllData[AllData$Bldg.Type=="1Fam",]

head(AllData)

AddData$BuildingAge = 2010 - AllData$Year.Built

RPerm <- sample(nrow(AllData))
AllData <- AllData[RPerm,]
head(AllData)
TrainInd <- ceiling(nrow(AllData)/2)
ValInd <- ceiling(nrow(AllData)/4)+TrainInd

TrainData <- AllData[1:TrainInd,]
ValData <- AllData[(TrainInd+1):ValInd,]
TestData <- AllData[(ValInd+1):nrow(AllData),]

TrainData <- read.table("GradedHW1-Train-Data.csv",header=T,sep=",",
                        stringsAsFactors = F,na.strings="")


TestData <- read.table("GradedHW1-Test-Data.csv",header=T,sep=",",
                        stringsAsFactors = F,na.strings="")


ValData <- read.table("GradedHW1-Validation-Data.csv",header=T,sep=",",
                        stringsAsFactors = F,na.strings="")

TrainData$BuildingAge = 2010 - TrainData$Year.Built
TestData$BuildingAge = 2010 - TestData$Year.Built
ValData$BuildingAge = 2010 - ValData$Year.Built
nrow(TrainData)
nrow(ValData)
nrow(TestData)

sum(is.na(TrainData$Lot.Area))
sum(is.na(ValData$Lot.Area))
sum(is.na(TestData$Lot.Area))

sum(is.na(TrainData$Total.Bsmt.SF))
sum(is.na(ValData$Total.Bsmt.SF))
sum(is.na(TestData$Total.Bsmt.SF))

sum(is.na(TrainData$Gr.Liv.Area))
sum(is.na(ValData$Gr.Liv.Area))
sum(is.na(TestData$Gr.Liv.Area))

sum(is.na(TrainData$Full.Bath))
sum(is.na(ValData$Full.Bath))
sum(is.na(TestData$Full.Bath))

sum(is.na(TrainData$Bedroom.AbvGr))
sum(is.na(ValData$Bedroom.AbvGr))
sum(is.na(TestData$Bedroom.AbvGr))

sum(is.na(TrainData$Year.Built))
sum(is.na(ValData$Year.Built))
sum(is.na(TestData$Year.Built))

summary(AllData$Total.Bsmt.SF)
summary(AllData$Lot.Area)
summary(AllData$Gr.Liv.Area)
summary(AllData$Full.Bath)
summary(AllData$Bedroom.AbvGr)
summary(AllData$Year.Built)

hist(TrainData$Lot.Area)
hist(TrainData$Total.Bsmt.SF)
hist(TrainData$Gr.Liv.Area)
hist(TrainData$Full.Bath)
hist(TrainData$Bedroom.AbvGr)
hist(TrainData$BuildingAge)
typeof(AllData)
class(AllData)
if(!require("FNN")) { install.packages("FNN"); require("FNN") }
TrainDF <- data.frame(TrainData$Lot.Area, TrainData$Total.Bsmt.SF, TrainData$Gr.Liv.Area, TrainData$BuildingAge, TrainData$Full.Bath, TrainData$Bedroom.AbvGr)
ValDF <- data.frame(ValData$Lot.Area, ValData$Total.Bsmt.SF, ValData$Gr.Liv.Area, ValData$BuildingAge, ValData$Full.Bath, ValData$Bedroom.AbvGr)
y= TrainData$SalePrice
out <-knn.reg(train=TrainDF,test=ValDF,y=TrainData$SalePrice,k=20)
ypred <- matrix(out$pred, nrow= 606)
residuals= ypred - ValData$SalePrice
head(residuals)
library(dplyr)
RMSE(ypred, ValData$SalePrice)


library(ISLR)
install.packages("caret")
library(caret)

li = c()
View(TestData)


View(TrainDF)
TrainDF <- data.frame(TrainData$Lot.Area, TrainData$Total.Bsmt.SF, TrainData$Gr.Liv.Area, TrainData$BuildingAge, TrainData$Full.Bath, TrainData$Bedroom.AbvGr)
ValDF <- data.frame(ValData$Lot.Area, ValData$Total.Bsmt.SF, ValData$Gr.Liv.Area, ValData$BuildingAge, ValData$Full.Bath, ValData$Bedroom.AbvGr)
TestDF <- data.frame(TestData$Lot.Area, TestData$Total.Bsmt.SF, TestData$Gr.Liv.Area, TestData$BuildingAge, TestData$Full.Bath, TestData$Bedroom.AbvGr)

TrainDF2 <- data.frame(lapply(TrainDF, scale))
ValDF2 <- data.frame(scale(ValDF, center= lapply(TrainDF, mean), scale = lapply(TrainDF, sd)))
View(TrainDF2)
View(ValDF2)
for(x in c(1:40)){
  k <- x
  out <-knn.reg(train=TrainDF3,test=ValDF3,y=TrainData$SalePrice,k=k)
  ypred <- matrix(out$pred)
  residuals= ypred - ValData$SalePrice
  print(k)
  print(RMSE(ypred, ValData$SalePrice))
  li[k]=(RMSE(ypred, ValData$SalePrice))
}
plot(x=c(1:40), y = li)
min(li)


hist(TrainData$Lot.Area)
qqnorm(log(TrainData$Lot.Area))

TrainData.transform <- TrainData
ValData.transform <- ValData
TestData.transform <- TestData

TrainData.transform$Lot.Area <- log(TrainData$Lot.Area)
ValData.transform$Lot.Area <- log(ValData$Lot.Area)
TestData.transform$Lot.Area <- log(TestData$Lot.Area)

hist(TrainData$Total.Bsmt.SF)
qqnorm((TrainData$Total.Bsmt.SF)^(1/3))
qqnorm(log(TrainData$Total.Bsmt.SF))

TrainData.transform$Total.Bsmt.SF <- TrainData$Total.Bsmt.SF^(1/3)
ValData.transform$Total.Bsmt.SF <- ValData$Total.Bsmt.SF^(1/3)
TestData.transform$Total.Bsmt.SF <- TestData$Total.Bsmt.SF^(1/3)

qqnorm(TrainData$Gr.Liv.Area)
qqnorm(log(TrainData$Gr.Liv.Area))

TrainData.transform$Gr.Liv.Area <- log(TrainData$Gr.Liv.Area)
ValData.transform$Gr.Liv.Area <- log(ValData$Gr.Liv.Area)
TestData.transform$Gr.Liv.Area <- log(TestData$Gr.Liv.Area)

qqnorm(TrainData$BuildingAge)
qqnorm((TrainData$BuildingAge)^(1/3))

TrainData.transform$BuildingAge <- (TrainData$BuildingAge)^(1/3)
ValData.transform$BuildingAge <- (ValData$BuildingAge)^(1/3)
TestData.transform$BuildingAge <- (TestData$BuildingAge)^(1/3)

qqnorm(TrainData$Full.Bath)
TrainData.transform$Full.Bath<- TrainData$Full.Bath
ValData.transform$Full.Bath<- ValData$Full.Bath
TestData.transform$Full.Bath<- TestData$Full.Bath


qqnorm(TrainData$Bedroom.AbvGr)

TrainData.transform$Full.Bedroom.AbvGr<- TrainData$Bedroom.AbvGr
ValData.transform$Full.Bedroom.AbvGr<- ValData$Bedroom.AbvGr
TestData.transform$Full.Bedroom.AbvGr<- TestData$Bedroom.AbvGr

TrainDF.transform <- data.frame(TrainData.transform$Lot.Area, TrainData.transform$Total.Bsmt.SF, TrainData.transform$Gr.Liv.Area, TrainData.transform$BuildingAge, TrainData.transform$Full.Bath, TrainData.transform$Bedroom.AbvGr)
ValDF.transform <- data.frame(ValData.transform$Lot.Area, ValData.transform$Total.Bsmt.SF,ValData.transform$Gr.Liv.Area, ValData.transform$BuildingAge, ValData.transform$Full.Bath, ValData.transform$Bedroom.AbvGr)
TestDF.transform <- data.frame(TestData.transform$Lot.Area, TestData.transform$Total.Bsmt.SF,TestData.transform$Gr.Liv.Area, TestData.transform$BuildingAge, TestData.transform$Full.Bath, TestData.transform$Bedroom.AbvGr)
View(TrainDF.transform)
View(ValDF.transform)

TrainDF.std <- data.frame(lapply(TrainDF, scale))
ValDF3 <- data.frame(scale(ValDF.transform, center= lapply(TrainDF.transform, mean), scale = lapply(TrainDF.transform, sd)))

View(TrainDF3)
View(ValDF3)

library(caret)
for(x in c(1:40)){
  k <- x
  out <-knn.reg(train=TrainDF.transform,test=ValDF.transform,y=TrainData$SalePrice,k=k)
  ypred <- matrix(out$pred)
  print(k)
  print(RMSE(ypred, ValData$SalePrice))
  li[k]=(RMSE(ypred, ValData$SalePrice))
}
plot(x=c(1:40), y = li)
min(li)


TrainDF.transform.std <- data.frame(lapply(TrainDF.transform, scale))
ValDF.transform.std <- data.frame(scale(ValDF.transform, center= lapply(TrainDF.transform, mean), scale = lapply(TrainDF.transform, sd)))



for(x in c(1:40)){
  k <- x
  out <-knn.reg(train=TrainDF.transform.std,test=ValDF.transform.std,y=TrainData$SalePrice,k=k)
  ypred <- matrix(out$pred)
  print(k)
  print(RMSE(ypred, ValData$SalePrice))
  li[k]=(RMSE(ypred, ValData$SalePrice))
}
plot(x=c(1:40), y = li)
min(li)


TestDF.transform <- data.frame(TestData.transform$Lot.Area, TestData.transform$Total.Bsmt.SF,TestData.transform$Gr.Liv.Area, TestData.transform$BuildingAge, TestData.transform$Full.Bath, TestData.transform$Bedroom.AbvGr)
TestDF.transform.std <- data.frame(scale(TestDF.transform, center= lapply(TrainDF.transform, mean), scale = lapply(TrainDF.transform, sd)))
TestDF.std <- data.frame(scale(TestDF, center= lapply(TrainDF, mean), scale = lapply(TrainDF, sd)))

out <-knn.reg(train=TrainDF.transform,test=TestDF.transform,y=TrainData$SalePrice,k=3)
ypred.transform <- matrix(out$pred)
RMSE.transform <- RMSE(ypred.transform, TestData$SalePrice)

out <-knn.reg(train=TrainDF.transform.std,test=TestDF.transform.std,y=TrainData$SalePrice,k=15)
ypred.transform.std <- matrix(out$pred)
RMSE.transform.std <- RMSE(ypred.transform.std, TestData$SalePrice)

out <-knn.reg(train=TrainDF,test=TestDF,y=TrainData$SalePrice,k=12)
ypred <- matrix(out$pred)
RMSE <- RMSE(ypred, TestData$SalePrice)


out <-knn.reg(train=TrainDF.std,test=TestDF.std,y=TrainData$SalePrice,k=12)
ypred.std <- matrix(out$pred)
RMSE.std <- RMSE(ypred.std, TestData$SalePrice)
