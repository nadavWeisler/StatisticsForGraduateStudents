setwd("C:/Users/t-nweisler/Downloads/Varied_RMS_Analysis-main/Varied_RMS_Analysis-main")
dat <- read.csv("cleanedData67b.csv")
library(lme4)

dat$WhereWhat = as.factor(dat$WhereWhat)
dat$Hz = as.factor(dat$Hz)
dat$Inversion = as.factor(dat$Inversion)
dat$ID = as.factor(dat$ID)
dat = subset(dat,logRT > 1)

#dat_avg=ddply(dat, c("Hz","Inversion","WhereWhat","ID"),summarize,
#              logRT=mean(logRT))

#summary(aov(logRT ~ WhereWhat*Hz*Inversion + Error(ID/(WhereWhat*Hz*Inversion)), data = dat_avg))

model = (lmer(logRT ~ WhereWhat * Hz * Inversion + (1 | ID), dat))
summary(model)
hist(resid(model), breaks = 50)
AIC(model)