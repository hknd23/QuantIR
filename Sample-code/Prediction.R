  library(haven)
  library(car)

  raw.data <- read_dta('https://github.com/joshuaalley/cross-sectional-ols/raw/master/potter-tavits-2015/potter_tavits_data.dta')
  
  # drop outliers
  campaigns <- subset(raw.data, postenp < 9.2)
  
  
  # estimate Model 1 in Table 2
  library(arm)
  plot(campaigns$fundparity4,campaigns$postenp)
  abline(lm(postenp~fundparity4,data=campaigns))
  full<-lm(postenp ~ fundparity4
           + demyears
           + fed 
           + pres 
           + log(avemag) 
           + fract 
           + log(avemag):fract, 
           data=campaigns)
  summary(full)	

  plot(full$model$fundparity4,full$fitted.values)
  
  plot(full$model$fundparity4,full$model$postenp)  
  
  plot(full$fitted.values,full$model$postenp,xlim=c(0,10),ylim=c(0,10))  
  abline(a = 0, b = 1)
  
  avPlots(full) 
 predict(full)
fitted(full) 
