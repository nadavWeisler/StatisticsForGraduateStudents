# TA 6 - Multiple regression in R, Mediation, Multicollinearity, Coding, similarities to ANOVA

# Gal Chen - galrefa.chen@mail.huji.ac.il
require(ggplot2)
require(GGally)
require(mediation)
require(diagram)

# We start by loading the dataset on  big 5
d=read.csv("big_five_scores.csv")
d$sex=as.factor(d$sex) # we code sex as factor

cor(d[,c(4,6:10)])
ggpairs(d[,c(4,6:10)])+theme_bw()

# it seems that agreebleness predicts neuroticism
summary(lm(neuroticism_score~agreeable_score,d)) 
summary(lm(neuroticism_score~agreeable_score+sex,d))
# We control for "sex"
# The intuition here is that we look at the average slope of agreebleness on neuroticism within each level of "sex"
ggplot(d,aes(x=agreeable_score,y=neuroticism_score))+
  geom_point()+
  geom_smooth(method="lm")+facet_wrap(~sex)

####### Mediation

# What happens to this correlation when we control for conscientiousness?
ggpairs(d[,c(4,6:10)])+theme_bw()
summary(lm(neuroticism_score~conscientiousness_score+agreeable_score,d))
# Note the dramatic change in r-squared. 
# This means that in our data agreebleness doesn't predict much unique variance in neuroticism

# In this specific case, the reason is mediation.

# Mediation analysis includes 3 steps - IV-DV relationship, IV-mediator relationship, IV+mediator-DV relationship
# What we do here is ask "to what extent the effect of agreebleness on neuroticsm goes through the agreebleness-conscientiousness correlation?

# Step 1 - IV - DV
summary(m1<-lm(neuroticism_score~agreeable_score,d))

# Step 2 - IV - mediator
summary(m2<-lm(conscientiousness_score~agreeable_score,d))

# Step 3 - IV - DV while controlling for mediator
summary(m3<-lm(neuroticism_score~conscientiousness_score+agreeable_score,d))

# At this point we can stop and say that the mediation exist. however. we might want to quantify it
# indirect effect = DV-mediator beta * IV-mediator beta. 
# This is the part of the IV-DV effect that is mediated through the mediator
lapply(list(m1,m2,m3),coef)
coef(m2)[2]*coef(m3)[2] # indirect effect
coef(m2)[2]*coef(m3)[2]/coef(m1)[2] # proportion of effect that was indirect (mediated)

data <- c(0, round(coef(m2)[2],3), 0,
          0, 0, 0, 
          round(coef(m3)[2],3), 
          paste(round(coef(m1)[2],3),"(",round(coef(m3)[3],3),")"), 0)
M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
plot<- plotmat (M, pos=c(1,2), 
                name= c(m3$terms[[3]][[2]],m3$terms[[3]][[3]],m3$terms[[2]]), 
                box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)

# But how can we know the p-value of the indirect effect?

# Alternative/complementary analysis - estimate the significance with bootstrap -
#  sample with replacement, calculate
results = mediate(m2, m3, treat='agreeable_score',
                  mediator='conscientiousness_score', boot=T)
summary(results)
plot(results)

#                Estimate 95% CI Lower  95% CI Upper  p-value   
# ACME             -0.160       -0.312         -0.03    0.012 *   # Indirect effect mediator beta * IV-mediator beta
# ADE              -0.139       -0.338          0.09    0.214     # Direct effect - the effect of IV on DV *after* controlling for mediator effect
# Total Effect     -0.298       -0.529         -0.06    0.010 **  # Total effect - the original IV-DV uncontrolled relationship
# Prop. Mediated    0.535        0.129          1.79    0.022 *   # ACME/Total effect


######### Multicollinearity

d$new_var=d$conscientiousness_score+rnorm(100,0,0.02)
summary(model_no_multico<-lm(neuroticism_score~conscientiousness_score+agreeable_score,d)) 
summary(model_multico<-lm(neuroticism_score~new_var+conscientiousness_score+agreeable_score,d)) 
# What happened? Where did the effect go?
# Think about a situation in which you have a group of female tennis players and male basketball players.
# The men are 16cm higher than the women.
# You run a 2-way ANOVA with the factors sport*gender: how can you know where does the effect belong to?
# You can't. This is the same case here - when 2 or more predictors are strongly related,
# The linear model "splits" the effect between them and doesn't know where it really belongs.

# In the case that one predictor can be predicted by the other predictors, 
#  we would get high VIF indicating multicollinearity
# The standard is to avoid cases of VIF over 5 - wither unify related variables or omit them
car::vif(model_multico)
car::vif(model_no_multico)


########### Coding - What happens when we include a categorical variable in the model?

d$sex
contrasts(d$sex) # Dummy coding
# Note that the default coding means that the first level serves as reference:
summary(lm(extraversion_score~sex,d)) # the intercept is the mean of one category, and we add B_1 for the second category
mean(d$extraversion_score[d$sex==1])

contrasts(d$sex)[1]=-1
summary(lm(extraversion_score~sex,d)) # same effect, but now the intercept reflects something else: 
#  the mean when sex==0
# In the case of balanced groups, this would be the grand mean

# When we have more than two levels, we need to add variables to code for all the levels:
data("happy")
happy=happy[1:200,] #sub sampling
head(happy)
levels(happy$marital)
contrasts(happy$marital) # "married" serves as reference, so beta will be indicative of the difference from "married"
summary(lm(wtssall~marital,happy))

happy$marital = relevel(happy$marital,ref="never married") # only for dummy coding
contrasts(happy$marital) # Now the reference is "never married" and we get different results
summary(lm(wtssall~marital,happy))
# Note that r squared and the significance of the model did not change!

# With more than one predictor, coding schemes might change the significance of the effects of interest
# With interactions, which we will see next week, it is even more important.

# These kinds of manipulations can get us further insight into categorical effects, and will become even more crucial next week when we add interactions.


# The last model we ran includes a categorical IV and a continuous DV. Sounds familiar?


######## Similarity to ANOVA/ttest
summary(aov(wtssall~marital,happy)) 
summary(lm(wtssall~marital,happy)) 

summary(lm(wtssall~marital+age,happy)) # Working directly with linear model allows controlling for other variables
anova(lm(wtssall~marital+age,happy)) # In the case I want to look at the general "marital" effect while controlling for age
# the case above is also termed ANCOVA in some cases. 

