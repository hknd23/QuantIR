install.packages('VGAM')
library(VGAM)

nes92<-read.csv('https://raw.githubusercontent.com/hknd23/PLSC-309-SP23/main/Data/1992election.csv')
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))

nes92.mlogit<-vglm(PVote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=2),data=nes92) 
summary(Clinton.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

Bush.nes92.mlogit2<-vglm(formula=presvote~partyid+age+fincome+female+white, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit2)

Clinton.nes92.mlogit2<-vglm(formula=presvote~partyid+age+fincome+female+white,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit2)


predictvglm(Bush.nes92.mlogit2,type='response')
predictvglm(Clinton.nes92.mlogit2,type='response')


plotvgam(Bush.nes92.mlogit2)
  #library(stargazer)
#stargazer(Bush.nes92.mlogit2,Clinton.nes92.mlogit2,out='mnl.htnml')