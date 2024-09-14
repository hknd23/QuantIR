

library(devtools)
install_github("jamesmartherus/anesr")
library(anesr)
library(effects)

library(MASS)
library(VGAM)

#some interesting variables
preelectvars<-c('V161003','V161005',
                'V161096','V161113',
                'V161139','V162281')

#rename these variables

names<-c('attention','voted2012','ftGOP',
         'prohealthcare','economy','whoinpower')

#import 2016 data
data('timeseries_2016')

#subset data by variable lists and rename 
anes2016<-timeseries_2016[,preelectvars]
colnames(anes2016)<-names

#replace negative values with missing
anes2016[anes2016< -5] <- NA
anes2016$voted2012a<-anes2016$voted2012-1
anes2016$inpower<-factor(anes2016$whoinpower)
MASSmodel <- polr(inpower ~ economy + voted2012a + attention +prohealthcare+ftGOP, data = anes2016, Hess=TRUE)
VGAMmodel <- vglm(inpower ~ economy + voted2012a + attention +prohealthcare+ftGOP, data = anes2016, family = cumulative(link = "logit", parallel = TRUE, reverse = TRUE))
OLSmodel<-lm(whoinpower ~ economy + voted2012 + attention +prohealthcare+ftGOP, data = anes2016)

predict.glm(MASSmodel,type='response')
MASSmodel$fitted.values
predict(MASSmodel,type='probs')

plot(effect("economy", MASSmodel), style = "stacked")
plot(effect("voted2012a", MASSmodel), style = "lines")
