#install.packages("lessR")
#install.packages("ggplot2")
library(ggplot2)
library(lessR)

mydata<-data_medical_charges
summary(mydata)
y=mydata$charges
x=mydata$age

###histogram on Age

hist(x)
h<-hist(x, breaks=10, col="gray", xlab="Age", 
        main="Histogram with Normal Curve") 

xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 

lines(xfit, yfit, col="black", lwd=2)



###histogram on purchase
hist(y)

h<-hist(y, breaks=10, col="gray", xlab="Charges", 
        main="Histogram of the Charges") 

xfit<-seq(min(y),max(y),length=40) 
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y)) 
yfit <- yfit*diff(h$mids[1:2])*length(y) 

lines(xfit, yfit, col="black", lwd=2)




## Pie chart of gender

gender <- data.frame(Gender = mydata$sex)
PieChart(Gender, hole = 0, values = "%", data = gender,
         fill = c("violet", "black"))


### Bar plot of the city category

table(mydata$City_Category)
ct<-table(mydata$City_Category)
barplot(ct,
        main = "City Category",
        xlab = "City type", ylab = "Number of people",
        col = c("darkgrey", "darkblue", "red"),
        legend.text = rownames(ct),
        args.legend = list(x = "topright",
                           inset = c(- 0.15, 0)),
        space = c(0.4, 2.5))

###### Bar plot of the length of Stay in current city
st<-table(mydata$region)
barplot(st,
        main = "City Category",
        xlab = "City type", ylab = "Number of people",
        col = c("darkgrey", "darkblue", "red","green","yellow"),
        legend.text = rownames(st),
        args.legend = list(x = "topright",
                           inset = c(- 0.15, 0)),
        space = c(0.4, 2.5))

### percenage marital status
table(mydata$Marital_Status)


### Fitting model of the charges on age

pmodel<-lm(y~mydata$age+mydata$sex+mydata$bmi+mydata$children+mydata$region,data=mydata)
summary(pmodel)


qqnorm( pmodel$residuals )
qqline( pmodel$residuals )

### Residual vs fitted
plot(pmodel, which=1)

## Q-Q Plot
plot(pmodel, which=2)

### Homoskedasticity 
plot(pmodel, which=3)

### Cook's distance plot 
plot(pmodel, which=4)

