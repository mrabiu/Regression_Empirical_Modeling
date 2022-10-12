library(MASS)
#library(party)
library(partykit)
#library(C50)
library(rpart)
library(randomUniformForest)
str(ConcreteCompressiveStrength, vec.len = 3)
#
source("TVHsplit.R")
source("ValidationRsquared.R")
source("PredictedVsObservedPlot.R")
#
#Renaming variables
concreteData <- ConcreteCompressiveStrength
colnames(concreteData) <- c("Cement", "BFslag",
                            "FlyAsh", "Water", "SuperP",
                            "Coarse", "Fine", "Age", "Strength")
#
# Split data
concreteFlag <- TVHsplit(concreteData, split = c(0.5, 0.5),
                         labels = c("T", "V"))
concreteTrain <- concreteData[which(concreteFlag == "T"), ]
concreteValid <- concreteData[which(concreteFlag == "V"), ]
#
#
lmConcrete <- lm(Strength ~ ., data = concreteTrain)
summary(lmConcrete)
lmConcreteHatV <- predict(lmConcrete, newdata = concreteValid)
#
lmTreeConcrete <- lmtree(Strength ~ . - Age | Age, data = concreteTrain)
summary(lmTreeConcrete)
lmTreeConcreteHatV <- predict(lmTreeConcrete, newdata = concreteValid)
#
ValidationRsquared(concreteValid$Strength, lmConcreteHatV)
#
ValidationRsquared(concreteValid$Strength, lmTreeConcreteHatV)
#
# 
par(mfrow=c(1,2))
PredictedVsObservedPlot(concreteValid$Strength,lmConcreteHatV, main='linear regression')
PredictedVsObservedPlot(concreteValid$Strength,lmTreeConcreteHatV, main ='MOB model')
#
# To save and restore all settable graphical parameters use:
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
par(oldpar)
