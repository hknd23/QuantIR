
install.packages('pROC')
library(pROC)

FL2003<-read.csv('https://raw.githubusercontent.com/hknd23/PLSC-309-SP23/main/Data/FL2003.csv')


modela <- glm(onset~mountains + priorwar + gdpen,
              data=FL2003, family="binomial")
summary(modela)

modelfull <- glm(onset~priorwar +gdpen+pop +mountains + noncontig+oil
                 +newstate+instability + polity2 +empethfrac+relfrac,
                 data=FL2003, family="binomial")
summary(modelfull)

preds <- predict(modela, type = "response")
test_roc <- roc(modela$model$onset ~ preds, plot = TRUE, print.auc = TRUE)

preds_full <- predict(modelfull, type = "response")
test_roc_full <- roc(modelfull$model$onset ~ preds_full, plot = TRUE, print.auc = TRUE)

#Smaller AIC ~ better fit

