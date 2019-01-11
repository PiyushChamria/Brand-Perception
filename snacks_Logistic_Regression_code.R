setwd("C:/Users/pc/Documents/R Project")

snacks<-read.csv("snacks.csv",header = T, stringsAsFactors = T)

summary(snacks)

table(snacks$X23) #Dependent Variable (Brand A perception)
table(snacks$X2)  #Independent Variable (Farm Ingredients)
table(snacks$X9)  #Independent Variable (Zero Gram Trans Fat)
table(snacks$X16) #Independent Variable (Natural Oil)
table(snacks$X30) #Independent Variable (Level Of Processing)

#As Dependent Variable (X23) is a Categorical Variable we'll use Logistic Regression to predict Brand A perception

###Data Preparation

which(is.na(snacks[,c(3,10,17,24,31)]))

str(snacks[,c(3,10,17,24,31)])

#X23 represents levels when greater than 5 representing Good Perception and less than or equal to 5 representing Bad Perception

snacks$Target<-ifelse(snacks$X23>4,1,0)

#X30 represemts levels Minimally Processed or Heavily Processed

snacks$Level_of_Processing<-ifelse(snacks$X30<=5,"Heavily Processed","Minimally Processed")

snacks1<-snacks[,c(3,10,17,31,62)]

snacks2<-snacks[,c(62,31)]

snacks2$X2_2<-ifelse(snacks1$X2==2,1,0)
snacks2$X2_1<-ifelse(snacks1$X2==1,1,0)
snacks2$X9_2<-ifelse(snacks1$X9==2,1,0)
snacks2$X9_1<-ifelse(snacks1$X9==1,1,0)
snacks2$X16_2<-ifelse(snacks1$X16==2,1,0)
snacks2$X16_1<-ifelse(snacks1$X16==1,1,0)

library(gains)
library(dplyr)
library(irr)
library(caret)
library(e1071)

###Logistics Regression Model on Object snacks1
mod<-glm(Target~.,data=snacks2,family="binomial")
summary(mod)

#We'll take out insignificant columns X2_1, X9_1x X16_1 out of dataset and run logistics regression on them again

snacks2<-snacks2[,-c(4,6,8)]

colnames(snacks2)<-c("Target","X30_ProcesingLevel","X2_2_FarmIngredients","X9_2_ZeroGramFat","X16_2_NaturalOil")

mod1<-glm(Target~.,data=snacks2,family="binomial")
summary(mod1)

#Summary(mod1) shows that all the variables are significant and they are correct in terms of their signs.
#Thus we have mod1 as our final model. 

#Next, we will gauge the performance of this model by looking at the performance metrics.

pred<-predict(mod1,type="response",newdata=snacks2)
head(pred)

table(snacks2$Target)/nrow(snacks2)

pred<-ifelse(pred>=0.5,1,0)
head(pred)

kappa2(data.frame(snacks2$Target,pred))

confusionMatrix(pred,snacks2$Target,positive="1")

gains(snacks2$Target,predict(mod1,type="response",newdata=snacks2),groups = 10)

snacks2$prob<-predict(mod1,type="response",newdata=snacks2)

quantile(snacks2$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

##Gain Chart tells that top 51% of the Probabilities contain 72% of customers Likes and believes that "Brand A" Chips are Average
##We normally expect top 30% probabilities to contain 70% of Customers

snacks2%>%group_by(X2_2_FarmIngredients)%>%
  summarise(Count=n(),Percent_Count = n()/nrow(snacks2))%>%ungroup()%>%data.frame()
#Here nearly 80% people beleive BrandA snacks are made with Farm Grown Ingredients

snacks2%>%group_by(X9_2_ZeroGramFat)%>%
  summarise(Count=n(),Percent_Count = n()/nrow(snacks2))%>%ungroup()%>%data.frame()
#Here Close to 32% Customers believe BRAND A Chips have zero grams trans-fat.

snacks2%>%group_by(X16_2_NaturalOil)%>%
  summarise(Count=n(),Percent_Count = n()/nrow(snacks2))%>%ungroup()%>%data.frame()
#Here Close to 44% Customers believe BRAND A Chips are made with natural oils.

processedRate <- snacks2%>%group_by(X30_ProcesingLevel)%>%
  summarise(Count=n(),Percent_Count = n()/nrow(snacks2)*100)%>%data.frame()%>%ungroup()
#Only 25% people have rated Level of Procesisng >5 that means 75% people rated below 5 and beleive that the 
#Brand A snacks are highly processed.

snacks%>%group_by(X38)%>%
  summarise(Count=n(),Percent_Count = n()/nrow(snacks)*100)%>%ungroup()%>%data.frame()
#Only 44% people have rated Level of Procesisng >5 that means 56% people rated below 5 and beleive that the 
#Brand A snacks are not Environmentally Responsible.
