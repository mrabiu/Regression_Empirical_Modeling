library(mlbench)
library(MASS)
data(PimaIndiansDiabetes)
#
#
source("TVHsplit.R")
source("BasicSummary.R")
#
#
BasicSummary(PimaIndiansDiabetes)
keepVars <- c("pregnant", "glucose", "pressure", "mass", "pedigree", "age")
PimaSub <- PimaIndiansDiabetes[, keepVars]
diabetesFlag <- ifelse(PimaIndiansDiabetes$diabetes == "neg", 0, 1)
PimaSub$diabetes <- diabetesFlag
TVflag <- TVHsplit(PimaSub, split = c(0.5, 0.5), labels = c("T", "V"))
PimaTrain <- PimaSub[which(TVflag == "T"), ]
PimaValid <- PimaSub[which(TVflag == "V"), ]
#
#
logisticFull <- glm(diabetes ~ ., data = PimaTrain, family = "binomial")
#
summary(logisticFull)
#
logisticRef <- glm(diabetes ~ glucose+mass, data = PimaTrain, 
                   family = "binomial")
#
summary(logisticRef)
#
logisticNull <- glm(diabetes ~ 1, data = PimaTrain, family = "binomial")
summary(logisticNull)
#
#
pHatFullT <- predict(logisticFull, newdata = PimaTrain, type = "response")
pHatFullV <- predict(logisticFull, newdata = PimaValid, type = "response")
pHatRefT <- predict(logisticRef, newdata = PimaTrain, type = "response")
pHatRefV <- predict(logisticRef, newdata = PimaValid, type = "response")
pHatNullT <- predict(logisticNull, newdata = PimaTrain, type = "response")
pHatNullV <- predict(logisticNull, newdata = PimaValid, type = "response")
#



