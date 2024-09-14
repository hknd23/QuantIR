library(haven)
library(margins)
library(sjPlot)
library(devtools)
install_github('hknd23/MarginalEffectsPlots')
library(MarginalEffectsPlots)
xenoslav<-read_dta("https://github.com/hknd23/PLSC-309-SP23/raw/main/Data/alexseev.dta")

model1<-lm(xenovote~slavicshare+changenonslav+ eduhi02+
             unemp02, data= xenoslav)


xenoslav$slavic_high<-ifelse(xenoslav$slavicshare>55,1,0)

modelb<-lm(xenovote~slavic_high*changenonslav+ eduhi02+
             unemp02, data= xenoslav)
summary(modelb)
meggplotdum(modelb,"changenonslav","slavic_high")
meggplot(modelb,"slavic_high","changenonslav")

modela<-lm(xenovote~slavicshare*changenonslav+ eduhi02+
            unemp02, data= xenoslav)
meggplot(modela,"changenonslav","slavicshare")
meggplot(modela,"slavicshare","changenonslav")
