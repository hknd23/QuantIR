FL2003<-read.csv('https://raw.githubusercontent.com/hknd23/PLSC-309-SP23/main/Data/FL2003.csv')

plot(FL2003$gdpen,FL2003$onset)
plot(FL2003$mountains,FL2003$onset)

modela <- glm(onset~mountains + priorwar + gdpen,
              data=FL2003, family="binomial")
summary(modela)

predict(modela, data.frame(mountains=0 , priorwar=1 ,
                             gdpen=3),  type="response")

predict(modela, data.frame(mountains=2 , priorwar=0 ,
                             gdpen=0.04), type="response")

round(predict(modela,type='response'),2)

plot(modela$model$priorwar, round(predict(modela,type='response'),2))

FL2003$gdpen

modelfull <- glm(onset~priorwar +gdpen+pop +mountains + noncontig+oil
                 +newstate+instability + polity2 +empethfrac+relfrac,
              data=FL2003, family="binomial")
summary(modelfull)

exp(modelfull$coefficients)

plot(modelfull$model$empethfrac,round(predict(modelfull,type='response'),2))

#Plotting Logit Curve

library(ggplot2)

ggplot(FL2003, aes(x=gdpen, y=onset)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

