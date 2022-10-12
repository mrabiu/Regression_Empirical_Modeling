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
lmConcrete <- lm(Strength ~ ., data = concreteTrain)
summary(lmConcrete)
#
rpartConcrete <- rpart(Strength ~ ., data = concreteTrain)
plot(rpartConcrete, main='Concrete Strenght')
text(rpartConcrete, xpd = TRUE, minlength = 5)
#
#
lmConcreteHatV <- predict(lmConcrete, newdata = concreteValid)
ValidationRsquared(concreteValid$Strength, lmConcreteHatV)
#
rpartConcreteHatV <- predict(rpartConcrete, newdata = concreteValid)
ValidationRsquared(concreteValid$Strength, rpartConcreteHatV)
#
par(mfrow=c(1,2))
PredictedVsObservedPlot(concreteValid$Strength,lmConcreteHatV, main='linear regression')
PredictedVsObservedPlot(concreteValid$Strength,rpartConcreteHatV, main ='regression tree')
#
# To save and restore all settable graphical parameters use:
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
par(oldpar)



