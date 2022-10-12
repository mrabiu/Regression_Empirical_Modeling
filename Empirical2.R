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
PimaPregnant1 <- glm(pregnant ~ ., data = PimaTrain, family = poisson)
summary(PimaPregnant1)
#
#
PimaPregnant2 <- glm(pregnant ~ ., data = PimaTrain, family = poisson(link = "identity"))
summary(PimaPregnant2)
#
#
PimaPregnant3 <- glm(pregnant ~ ., data = PimaTrain,
                     family = poisson(link = "sqrt"))
summary(PimaPregnant3)
