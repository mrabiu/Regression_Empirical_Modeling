library(MASS)
#library(party)
#library(partykit)
#library(C50)
library(rpart)
#
#
data(whiteside) # from the MASS package
#
par(mfrow=c(1,2))
rpartWhiteA <- rpart(Gas ~ Insul, data = whiteside)
plot(rpartWhiteA, main='Gas ~ Insul')
text(rpartWhiteA, xpd = TRUE, minlength = 5)
#
rpartWhiteA <- rpart(Gas ~ Temp, data = whiteside)
plot(rpartWhiteA, main='Gas ~ Templ')
text(rpartWhiteA, xpd = TRUE, minlength = 5)
#
# To save and restore all settable graphical parameters use:
oldpar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
par(oldpar)

#
#
mean(whiteside$Gas[which(whiteside$Insul == "Before")])
##
mean(whiteside$Gas[which(whiteside$Insul == "After")])
#
#
rpartWhiteB <- rpart(Gas ~ Temp, data = whiteside)
print(rpartWhiteB)



