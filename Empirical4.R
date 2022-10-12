library(MASS)
library(rpart)
#
#
data(whiteside) # from the MASS package
#
#
rpartWhiteC <- rpart(Gas ~ Temp + Insul, data = whiteside)
plot(rpartWhiteC, main='Gas ~ Temp + Insul')
text(rpartWhiteC, xpd = TRUE, minlength = 5)
print(rpartWhiteC)