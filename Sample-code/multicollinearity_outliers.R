library(olsrr)
library(car)
dataset<-read.csv('https://github.com/PrisonRodeo/PLSC503-2023-git/raw/main/Exercises/PLSC503-2023-ExerciseFour.csv')
#cor(dataset[,6:ncol(dataset)],use='pairwise')

rownames(dataset)<-dataset$country

WDIModel<-lm(GovtExpenditures ~ 
           PopGrowth
         + AgriEmployment 
         + log(CO2Emissions) 
         +GDPPerCapita+ Unemployment+Inflation, 
         data=dataset)

summary(WDIModel)	
#round(vcov(WDIModel),4)
cor(WDIModel$model)
pairs(WDIModel$model)
vif(WDIModel)

plot(WDIModel)


dfbetas(WDIModel)
cutoff<-2/sqrt(nrow(WDIModel$model))

modeldata<-WDIModel$model

modeldata$highdfbeta<-rowSums( dfbetas(WDIModel)>cutoff)

modeldata[modeldata$highdfbeta>0,]


dataset[192,]
dataset[110,]

pairs(WDIModel$model)

Boxplot(dataset$FertilityRate)
Boxplot(dataset$BirthRatePer1K)
Boxplot(dataset$PopGrowth)

dataset[c(121,44,39,142),]
