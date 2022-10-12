library(MASS)
#library(party)
library(partykit)
#library(C50)
library(rpart)
#
#
data(whiteside) # from the MASS package
#
lmTreeWhite <- lmtree(Gas ~ Temp | Insul, data = whiteside)
plot(lmTreeWhite)
print(lmTreeWhite)