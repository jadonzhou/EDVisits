library(lme4)
library(lmmlasso)
library(foreign)
library(multcomp)


rm(list=ls())

R2c<-c()
RMSEc<-c()


setwd("C:/Users/jurata/Desktop/backup/opioids/ED/")
d <- read.csv(file=paste("ED.csv",sep=""), head=TRUE,sep=",")
d$LOGRATE <- log(d$RATE)
exclude <- c('2004','2009','2010','2011','2012','2013','2014','2015','2016','2017')
dsub <- subset(d, !(YEAR %in% exclude))
drops <- c("BIG.H","HELL.DUST","MURDER.8","SPEED.BALLING")
dsub <- dsub[ , !(names(dsub) %in% drops)]
#dropcity <- c('New York','San Francisco')
#dsub <- subset(dsub, !(GEO %in% dropcity))



testyears <-c('2009','2010','2011')
dtest <- subset(d, (YEAR %in% testyears))
#dtest <- subset(dtest, !(GEO %in% dropcity))

hist(dsub$ESTIMATE)
hist(log(dsub$ESTIMATE))
hist(dsub$RATE)
hist(log(dsub$RATE))

plot(density(dsub$ESTIMATE))
plot(density(log(dsub$ESTIMATE)))
qqnorm(dsub$ESTIMATE);qqline(dsub$ESTIMATE, col = 2)
qqnorm(log(dsub$ESTIMATE));qqline(log(dsub$ESTIMATE), col = 2)

plot(density(dsub$RATE))
plot(density(log(dsub$RATE)))
qqnorm(dsub$RATE);qqline(dsub$RATE, col = 2)
qqnorm(log(dsub$RATE));qqline(log(dsub$RATE), col = 2)

shapiro.test(dsub$ESTIMATE); shapiro.test(log(dsub$ESTIMATE))
shapiro.test(dsub$RATE); shapiro.test(log(dsub$RATE))



#t01 <- subset(d,Week <= 11 )


m1 <- lmer(dsub$LOGRATE ~
             FENTANYL+
             OPIOID+
             AVINZA+
             CHINA.WHITE+
             GOODFELLA+
             GREY.DEATH+
             WHITE.HORSE+
             HEROIN+
             HYDROCODONE+
             APACHE+
             # #APACHE+
             # AVINZA+
             # #BLACK.STUFF+
             # #BLACK.TAR+
             # #BROWN.SUGAR+
             # #CHINA.WHITE+
             # CODEINE+
             # #DANCE.FEVER+
             # DOPE+
             # FENTANYL+
             # #GOLDEN.GIRLS+
             # #GOODFELLA+
             # #GREY.DEATH+
             # HEROIN+
             # #HILLBILLY.HEROIN+
             # HYDROCODONE+
             # KADIAN+
             # MEPERIDINE+
             # METHADONE+
             # MORPHINE+
             # OPIOID+
             # OXYCODONE+
             # #OXYMORPHONE+
             # #SPEEDBALLING+
             # TRAMADOL+
             # #WHITE.HORSE+
              (1|GEO) + (1|YEAR),data=dsub)




g = dsub$GEO


x <- model.matrix(m1, type="fixed")


y <- as.matrix(dsub$LOGRATE)


z <- as.matrix(x[,1])


par(mfrow=c(1,1))

bic <- rep(0, 30)
for (i in 1:30) {
  print(i)
  l <- lmmlasso(x=x, y=y, z=z, grp=g, lambda=(i), standardize=T)
  bic[i] <- l$bic
}
which.min(bic)
plot(bic)



lmm <- lmmlasso(x=x, y=y, z=z, grp=g, lambda=which.min(bic), standardize=T)


rlist<-c()
RMSElist<-c()
for (i in 2009:2011) {
  
  par(mfrow=c(1,1))
  v<-subset(dtest,YEAR==i)
  newv <- subset(dtest,YEAR==i, select=names(coef(lmm))[-1])
  newv <- cbind(rep(1,nrow(v)), newv)
  fixef_pred <- as.matrix(newv) %*% coef(lmm)
  fitted <- fixef_pred + lmm$ranef
  

  
  backTrans <- exp(fitted)*exp(sum(lmm$residuals^2)/(nrow(dsub)-2))
  #Corlist<-c(Corlist,cor(v$LOGRATE ,backTrans))
  RMSElist<-c(RMSElist,sqrt(mean((v$LOGRATE-backTrans)^2)))
  
  SST <- sum((v$LOGRATE - mean(v$LOGRATE))^2)
  
  lm<-summary(lm(v$LOGRATE~backTrans))
  cat("\nYear:", i,"\n")
  #print(1- sum(lm$residuals^2)/SST)
  #print(lm$r.squared)
  #print(cor(v$LOGRATE ,backTrans)*cor(v$LOGRATE ,backTrans))
  
  

  X <- cbind(rep(1,nrow(v)), backTrans)
  fittedvals <- X%*%lm$coefficients[,1]
  SSE <- sum((v$LOGRATE-fittedvals)^2)
  rlist<- c(rlist,(1 - SSE/SST))
  
  
}

R2<-rlist
R2c<-c(R2c,R2)
RMSEc<-c(RMSEc,RMSElist)
par(mfrow=c(1,1))
plot((1:3),R2,xlab="Week Number",ylab="R^2")
lines((1:3),R2,type="o")
title("R^2 by Week 11 through 62")
text(1.3,.65,paste("Mean: ",formatC(mean(R2),width=2,flag="0")))
text(1.3,.63,paste("Stdev: ",formatC(sd(R2),width=2,flag="0")))
mean(R2)
sd(R2)

par(mfrow=c(1,1))
plot((1:3),RMSElist,xlab="Week Number",ylab="RMSE")
lines((1:3),RMSElist,type="o")
title("RMSE by Week 11 through 62")
text(1.3,150.57,paste("Mean: ",formatC(mean(RMSElist),width=2,flag="0")))
text(1.3,150.55,paste("Stdev: ",formatC(sd(RMSElist),width=2,flag="0")))
mean(RMSElist)
sd(RMSElist)

cbind(v$DMA,v$RATE,backTrans,(v$RATE-backTrans)/v$RATE)
summary(lmm)
