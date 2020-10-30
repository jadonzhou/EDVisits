library(lme4)
library(lmmlasso)
library(foreign)
library(multcomp)


rm(list=ls())

R2c<-c()
RMSEc<-c()


setwd("C:/Users/jurata/Desktop/backup/opioids/ED/")
d <- read.csv(file=paste("ED.csv",sep=""), head=TRUE,sep=",")
d$LOGRATE <- d$RATE#log(d$RATE)
dsub <- d[complete.cases(d),]
drops <- c("BIG.H","HELL.DUST","MURDER.8","SPEED.BALLING")
dsub <- dsub[ , !(names(dsub) %in% drops)]


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
             #APACHE+
             AVINZA+
             #BLACK.STUFF+
             #BLACK.TAR+
             #BROWN.SUGAR+
             #CHINA.WHITE+
             CODEINE+
             #DANCE.FEVER+
             DOPE+
             FENTANYL+
             #GOLDEN.GIRLS+
             #GOODFELLA+
             #GREY.DEATH+
             HEROIN+
             #HILLBILLY.HEROIN+
             HYDROCODONE+
             KADIAN+
             MEPERIDINE+
             METHADONE+
             MORPHINE+
             OPIOID+
             OXYCODONE+
             #OXYMORPHONE+
             #SPEEDBALLING+
             TRAMADOL+
             #WHITE.HORSE+
              (1|GEO) + (1|YEAR),data=dsub)




g = dsub$GEO


x <- model.matrix(m1, type="fixed")


y <- as.matrix(dsub$LOGRATE)


z <- as.matrix(x[,1])


par(mfrow=c(1,1))

bic <- rep(0, 50)
aic <- rep(0, length(bic))
for (i in 1:length(bic)) {
  print(i)
  l <- lmmlasso(x=x, y=y, z=z, grp=g, lambda=(i), standardize=F)
  bic[i] <- l$bic
  aic[i] <- l$aic
}
which.min(bic)
which.min(aic)
plot(bic,type="o")
plot(aic,type="o")



lmm <- lmmlasso(x=x, y=y, z=z, grp=g, lambda=which.min(bic), standardize=F)
plot(lmm)
summary(lmm)
lmm

lmm2 <- lmmlasso(x=x, y=y, z=z, grp=g, lambda=which.min(aic), standardize=F)
plot(lmm2)
summary(lmm2)
lmm2


cor.test(d$RATE,d$HEROIN)
cor.test(d$RATE,d$HYDROCODONE)