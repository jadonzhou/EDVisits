library(glmmLasso)


rm(list=ls())



setwd("C:/Users/jurata/Desktop/backup/opioids/ED/")
d <- read.csv(file=paste("ED.csv",sep=""), head=TRUE,sep=",")

d$GEO <- as.factor(d$GEO)
d$YEAR <- as.factor(d$YEAR)

d$LOGRATE <- d$RATE#log(d$RATE)
dsub <- d[complete.cases(d),]
drops <- c("BIG.H","HELL.DUST","MURDER.8","SPEED.BALLING")
dsub <- dsub[ , !(names(dsub) %in% drops)]

dsub[,c(6:31)]<-scale(dsub[,c(6:31)],center=TRUE,scale=TRUE)
dsub<-data.frame(dsub)

# hist(dsub$ESTIMATE)
# hist(log(dsub$ESTIMATE))
# hist(dsub$RATE)
# hist(log(dsub$RATE))
# 
# plot(density(dsub$ESTIMATE))
# plot(density(log(dsub$ESTIMATE)))
# qqnorm(dsub$ESTIMATE);qqline(dsub$ESTIMATE, col = 2)
# qqnorm(log(dsub$ESTIMATE));qqline(log(dsub$ESTIMATE), col = 2)
# 
# plot(density(dsub$RATE))
# plot(density(log(dsub$RATE)))
# qqnorm(dsub$RATE);qqline(dsub$RATE, col = 2)
# qqnorm(log(dsub$RATE));qqline(log(dsub$RATE), col = 2)
# 
# shapiro.test(dsub$ESTIMATE); shapiro.test(log(dsub$ESTIMATE))
# shapiro.test(dsub$RATE); shapiro.test(log(dsub$RATE))


bic <- rep(0, 2000)
aic <- rep(0, length(bic))
for (i in 1:length(bic)) {
  print(i)
  l  <- glmmLasso(LOGRATE ~
                       APACHE+
                       AVINZA+
                       BLACK.STUFF+
                       BLACK.TAR+
                       BROWN.SUGAR+
                       CHINA.WHITE+
                       CODEINE+
                       DANCE.FEVER+
                       DOPE+
                       FENTANYL+
                       GOLDEN.GIRLS+
                       GOODFELLA+
                       GREY.DEATH+
                       HEROIN+
                       HILLBILLY.HEROIN+
                       HYDROCODONE+
                       KADIAN+
                       MEPERIDINE+
                       METHADONE+
                       MORPHINE+
                       OPIOID+
                       OXYCODONE+
                       OXYMORPHONE+
                       SPEEDBALLING+
                       WHITE.HORSE+
                       TRAMADOL,
                     lambda=i,
                     family = gaussian(link = "identity"),
                     rnd=list(GEO=~1,YEAR=~1),
                     data=dsub)
                     
  bic[i] <- l$bic
  aic[i] <- l$aic
}
which.min(bic)
which.min(aic)
plot(bic,type="o")
plot(aic,type="o")



lmm <- glmmLasso(LOGRATE ~
                      APACHE+
                      AVINZA+
                      BLACK.STUFF+
                      BLACK.TAR+
                      BROWN.SUGAR+
                      CHINA.WHITE+
                      CODEINE+
                      DANCE.FEVER+
                      DOPE+
                      FENTANYL+
                      GOLDEN.GIRLS+
                      GOODFELLA+
                      GREY.DEATH+
                      HEROIN+
                      HILLBILLY.HEROIN+
                      HYDROCODONE+
                      KADIAN+
                      MEPERIDINE+
                      METHADONE+
                      MORPHINE+
                      OPIOID+
                      OXYCODONE+
                      OXYMORPHONE+
                      SPEEDBALLING+
                      WHITE.HORSE+
                      TRAMADOL,
                    lambda=2100,#1975,#which.min(bic),
                    family = gaussian(link = "identity"),
                    rnd=list(GEO=~1,YEAR=~1),
                    data=dsub)
                    #control = list(print.iter=TRUE))

summary(lmm)
lmm
