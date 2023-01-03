# TA 7 - multiple regression with interactions, intro to GLM

library(ggeffects)
library(MASS)
library(ggplot2)
library(ggeffects)
library(ggpubr)

data("Pima.tr2")
# These data frames contains the following columns:
#   npreg
# number of pregnancies.
#   glu
# plasma glucose concentration in an oral glucose tolerance test.
#   bp
# diastolic blood pressure (mm Hg).
#   skin
# triceps skin fold thickness (mm).
#   bmi
# body mass index (weight in kg/(height in m)\^2).
#   ped
# diabetes pedigree function.
#   age
# age in years.
#   type
# Yes or No, for diabetic according to WHO criteria.
# 

# Interactions between continuous variables
summary(m1<-lm(bmi~age*glu,Pima.tr2)) # How to interpret this?
Pima.tr2$age_s=as.numeric(scale(Pima.tr2$age))
Pima.tr2$glu_s=as.numeric(scale(Pima.tr2$glu))
summary(m1<-lm(bmi~age_s*glu_s,Pima.tr2)) # scaled and centered
ggplot(Pima.tr2,aes(x=age,y=bmi, color=glu))+geom_point() # unclear and inaccurate

plot(ggpredict(m1,terms=c("age_s")))+ylim(25,40)+theme_classic() # no main effect for age
plot(ggpredict(m1,terms=c("glu_s")))+ylim(25,40)+theme_classic() # main effect for glu
plot(ggpredict(m1,terms=c("age_s","glu_s")))+theme_classic() # interaction
plot(ggpredict(m1,terms=c("age_s","glu_s[-3,-2,-1,0,1,2,3]")),ci=F,colors="viridis")+theme_classic() # interaction
# Those are predictions based on the model formula and differen values of age (all) and glu (mean+-1SD)
# Note that those are not three different groups! The CIs are based on all the data.

# Categorical - continuous interactions
n=150
set.seed(2)
sports <- round(rnorm(n=n, mean=5, sd = 2.5), digits = 1)
sports <- ifelse(sports > 0, sports, -sports)
sports_team <- rep(c("a","b"),n/2)
sports_team <- sample(sports_team)
happiness <- 6 +0.5*(sports_team=="b")+.1*sports+ 1.5*sports*(sports_team=="b") + rnorm(n=n, mean = 0, sd = 2)

d = data.frame(sports = sports, happiness = happiness, team_sports = sports_team)
d$team_sports=as.factor(d$team_sports)
rm(sports_team, happiness, sports)

summary(lm(happiness~sports*team_sports,d))
ggplot(data = d, aes(x = sports, y = happiness, color = team_sports)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  geom_rect(aes(xmin=-1,ymin=4,xmax=1,ymax=8),color="Black", fill=NA)+
  geom_rect(aes(xmin=-.5,ymin=5,xmax=11,ymax=7.5),color="darkgreen", fill=NA)+
  theme_bw(base_size = 18)
# The meaning depends heavily on coding and centering!
# Intercept: the expected value when sports==0 and when team_sports==a
# sports: the slope of sports when team_sports==a
# team_sportsb: the difference between a and b when sports==0
# The "main" effects are based on the red graph!

contrasts(d$team_sports)
contrasts(d$team_sports)[1]=-1
summary(lm(happiness~sports*team_sports,d))
# Intercept: the expected value when sports==0 (collapsed over the two levels of team_sports)
# sports: the average slope of sports between the two levels of team sports
# team_sportsb: the difference between a and b when sports==0
ggplot(data = d, aes(x = sports, y = happiness)) + 
  geom_point() + 
  geom_smooth(method = lm)

summary(m_centered_coded<-lm(happiness~scale(sports,scale=F,center=T)*team_sports,d))
# Intercept: the grand mean
# sports: the average slope of sports between the two levels of team sports
# team_sportsb: the difference between a and b at the average of sports
ggline(d,x="team_sports",y="happiness",add="mean_ci",ylim=c(0,20))

# Take home from this: When working with interactions, 
#  almost always we would prefer to center continuous variables and recode the categorical variables.



##### General linear models - intro
# glm - logistic & poisson examples
# glm is a function that works like lm, but it takes another argument - "family"
# this argument specifies the link function - instead of predicting y, we predict f(y)
# f() could be any sort of mathematical operation conducted on y

# Logistic link function for binary data
#   Our question - When controlling for mpg, does the weight predict whether the car is manual or automatic?
summary(m_logistic<-glm(am~scale(wt),mtcars,family="binomial")) 
ggplot(mtcars,aes(x=wt,y=am))+geom_point()+
  stat_smooth(method="glm",method.args=list(family="binomial"))
# what about effect size?
# One common measure of effect size is odds ratio, calculated by exp(coefficient), or e^coefficient.
# For each observation, we can calculate the estimated probability, p,
#   that their outcome variable will be "1"
# Odds are p/(1-p), the odds an event would happen.
# e.g. Odds of 4 means that the event is 4 times more likely to happen
# Odds ratio are the ratio of the odds at a certain level, compared to the odds at the intercept:

# OR = Odds_iv1/Odds_iv0

# if the odds are 4for IV=1 and 2 for IV=0, then the odds ratio will be 
1/round(exp(coef(m_logistic)),5) # inversed because coefficients are negative
# Increasing "wt" by 1 (SD, b/c it's scaled), makes the car 50 times less likely to be automatic

# What about the intercept?
# the intercept will be the odds that at a baseline level, the event will occur.
summary(m_logistic<-glm(am~1,mtcars,family="binomial")) #only intercept model
p_am=mean(mtcars$am)
p_am/(1-p_am) # manual calculation of the odds for outcome
exp(coef(m_logistic))
# When coded correctly, intercept of 0 indicates 50% for the event to occur.

#####
# Poisson link function for count data - we model our deviation as a function of poisson distribution
# The distribution has one parameter - lambda. 
# It represents the number of events that happen in a given timeframe
# i.e. the number of times a phone would ring in a day
# the number of errors subjects will make in a 5 minutes experiment
par(mfrow=c(3,1))
hist(rpois(1000,lambda=1),main= " Lambda=1",xlim=c(0,15),breaks=30)
hist(rpois(1000,lambda=3),main= " Lambda=3",xlim=c(0,15),breaks=30)
hist(rpois(1000,lambda=6),main= " Lambda=6",xlim=c(0,15),breaks=30)

d_foot=read.csv("football_data_ta7.csv")
ggplot(data=d_foot,aes(x=AerialsWon,y=Goals))+geom_point()+
  geom_smooth(method="lm")+stat_cor()
summary(m<-lm(Goals~AerialsWon,d_foot)) # insiginificant

hist(d_foot$Goals) # But goals is a count data
summary(m_poisson <- glm(Goals~AerialsWon,d_foot,family="poisson")) # significant!
# Possion regression is not always important when talking about count data, 
#  but it becomes more crucial when we have low numbers that deviate from normality (<10)


