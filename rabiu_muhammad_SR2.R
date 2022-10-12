pdata<-data_purchase_behaviour
purchase<-pdata$Purchase
age<-pdata$Age_num
gender<-pdata$Gender
city<-pdata$City_Category
mstat<-pdata$Marital_Status
stay<-pdata$Stay_In_Current_City_Years

model1<-lm(purchase~age+gender+city+mstat+stay,data=pdata)

summary(model1)


model2<-lm(purchase~age+gender+city,data=pdata)
summary(model2)


anova(model1,model2)

par(mar = rep(2, 4))

plot(model2,which=1)
plot(model2,which=2)
plot(model2,which=3)
plot(model2,which=4)

model3<-lm(purchase~age+gender,data=pdata)
summary(model3)

anova(model2,model3)
