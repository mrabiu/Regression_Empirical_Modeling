library(gbm)
library(MASS)
#library(party)
library(partykit)
#library(C50)
library(rpart)
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
#
lmConcrete <- lm(Strength ~ ., data = concreteTrain)
summary(lmConcrete)
lmConcreteHatV <- predict(lmConcrete, newdata = concreteValid)
#
#
gbmConcreteModel1 <- gbm(Strength ~ ., data = concreteTrain)
## Distribution not specified, assuming gaussian ...
gbmConcreteHatV1 <- predict(gbmConcreteModel1, newdata = concreteValid,
                            n.trees = 100)
ValidationRsquared(concreteValid$Strength, gbmConcreteHatV1)
ValidationRsquared(concreteValid$Strength, lmConcreteHatV)
#
#
par(mfrow=c(1,2))
PredictedVsObservedPlot(concreteValid$Strength,lmConcreteHatV, main='linear regression')
PredictedVsObservedPlot(concreteValid$Strength,gbmConcreteHatV1, main ='GBM Model')
#
# To save and restore all settable graphical parameters use:
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
par(oldpar)
#
#
gbmConcreteModel2 <- gbm(Strength ~ ., data = concreteTrain,
                         distribution = "gaussian",
                         interaction.depth = 30,
                         shrinkage = 0.05,
                         n.trees = 1000,
                        n.minobsinnode = 20)
#
#
gbmConcreteHatV2 <- predict(gbmConcreteModel2, newdata = concreteValid,
                            n.trees = 1000)
ValidationRsquared(concreteValid$Strength, gbmConcreteHatV2)
#
#
par(mfrow=c(1,2))
PredictedVsObservedPlot(concreteValid$Strength,gbmConcreteHatV1, main='Default GBM Model')
PredictedVsObservedPlot(concreteValid$Strength,gbmConcreteHatV2, main ='Tuned GBM Model')
#
# To save and restore all settable graphical parameters use:
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
par(oldpar)


