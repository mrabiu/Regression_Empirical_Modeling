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
rpartModel <- rpart(diabetes ~ glucose + mass, data = PimaTrain)
plot(rpartModel, main='Diabetes~glucose + mass')
text(rpartModel, xpd = TRUE, minlength = 5)
print(rpartModel)
#
rpartHatV <- predict(rpartModel, newdata = PimaValid)
