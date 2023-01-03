# TA 5 - rmANOVA ,Linear models in R

# Gal Chen - galrefa.chen@mail.huji.ac.il
require(reshape2)
require(emmeans)
library(ggpubr)
# One debt from class - how do we run rmANOVA?

# essentially, rmANOVA is very similar to between-subject anova, 
#  but we need to add another term to define the "within" factor
# Loading the dataset from: 
  # Siegel, E. H., Wormwood, J. B., Quigley, K. S., & Barrett, L. F. (2018).
  # Seeing what you feel: Affect drives visual perception of structurally neutral faces.
  # Psychological science, 29(4), 496-503.

# in short, people saw and rated the happiness of facial expression of faces
 # that were subliminally primed by one of three faces: happy, scowling, neutral

setwd("C:/Users/galche/Dropbox/psych-ma-research-methods/ta-gal/")
siegel_data = read.csv("siegel_etal.csv")
colnames(siegel_data)[1]="id" # fixing first colname
cor(siegel_data[,7:9]) # unsurprisingly, observations are strongly correlated
# subjects who rated high one face type, did so for the others

head(siegel_data) # the data is in "wide" format, while anova requires "long" format - one line for each observation
siegel_long <- melt(siegel_data, id.vars = c("id"),
                    measure.vars=c("Percept_mean.Neut","Percept_mean.Scowl","Percept_mean.Smile"), #the variables that will appear in one column
                    variable.name = "condition") %>% merge(siegel_data[,1:6],by="id") # merge for adding the demographic variables
siegel_long$id=as.factor(siegel_long$id) # we set the id to factor, so R won't treat it as a numeric variable 
table(siegel_long$id) # each id repeats three times


# And now, for the ANOVA!
summary(rm_aov<-aov(value~condition,data=siegel_long)) # between-subject - violation of independence assumption!
summary(rm_aov<-aov(value~condition+Error(id),data=siegel_long)) #within-subject

# But what if we (a-priori, we are not data-abusers here) think that gender might influence the effect?

#########

# In class assignment - complete the formula for the 2-way ANOVA
# summary(rm_aov2<-aov(value~???,data=siegel_long)) #2-way mixed (1-within/1-between)
# Note that the gender is a between-subject factor, so it is calculated against a different error term

#########

# Plotting a spaghetti plot
ggplot(data=siegel_long,aes(x=condition,y=value))+geom_point()+
  geom_line(aes(group=id))+
  stat_summary(fun = "mean", colour = "darkred", size = 4, geom = "point")+
  facet_wrap(~gender)
# The between-subject SE is not informative beacuse it is not used to calculate the significance of the result anymore!

#what about contrasts/post hoc?
#same execution as between-subject anova 

emm <- emmeans(rm_aov, ~condition)
pairs(emm,adjust="tukey")
emm2 <- emmeans(rm_aov2, ~condition*gender)
pairs(emm2,adjust="tukey",by=c("gender"))

##############
 
# The goal of the rest of this tutorial is to introduce you to the basics of the most useful tool R suggests for psychologists.
# In fact, we have used this tool implicitly so far, and we would keep using it in the future.

# Linear models are all kinds of models of the shape y~b0+b1*x1+b2*x2...bk*xk

# In R, we can create a model by using th lm() command

cars=mtcars # "mtcars" is a built-in data set with data about, well, cars
head(cars)

# let's predict miles per gallon by number of weight
cor.test(cars$wt,cars$mpg)


model1 = lm(mpg ~ wt) # Error! why?
?lm() #it wants not only a formula, but a dataset, otherwise it won't know where to get the data from

model1 = lm(mpg ~ wt,cars)
model1
# The output of the "lm" command are the coefficients that create the prediction formula: mpg=37.885-2.876*wt

# but this linear model contains much more information than "just" the coefficients:
View(model1)
# we can look at the residuals of the model and even plot them:
resid(model1)
hist(resid(model1))
#looking whether the data looks close to normality:

qqnorm(resid(model1))
qqline(resid(model1), col = "darkblue") # this graph, QQplot, draws the correlation between the histogram "heights" and the normal distribution
# the high and low observations are a little to extreme than what we would expect if the distribution was normal

# We also assume an equal variability of residuals along the regression line, this is hardly the result
# However, these violations are not severe

# another nice thing we can do is create a prediction very fast:

###########

# In-class assignment: use the manual way to calculate the predictions, and compare them
# betas=coef(model1)
# mpg_pred1= ??? 
# mpg_pred2=predict(???,cars) #easier
# mpg_pred1==mpg_pred2 # should be TRUE

###########

# what happens if we use another dataset (with the wt/mpg column) for the prediction model?
# it will allow us to see how well the model performs on new data

# the summary() command can calculate and get us the important details about our model:
summary(model1)
cor.test(predict(model1,cars), cars$mpg) # Testing whether the model predictions are correlated with the dependent variable


# Linear models allow numeric variables and factors, that are coded by dummy coding
data(iris)
head(iris)
class(iris$Species)
summary(iris_model<-lm(Sepal.Length~Species,iris))
# this is equivalent to:
iris$Speciesversicolor=0
iris$Speciesversicolor[iris$Species=="versicolor"]=1
iris$Speciesvirginica=0
iris$Speciesvirginica[iris$Species=="virginica"]=1
# Note that the two new variables are "numeric" although they contain only ones and zeros
head(iris[97:150,])
summary(iris_model_dummy<-lm(Sepal.Length~Speciesversicolor+Speciesvirginica,iris))

# More on coding next lesson...

# The anova() command runs an Analysis of Variance on models. it can be used to compare two models (more on that later)
  # or for running our beloved 1-way ANOVA test

anova(iris_model) # the two effects from before are presented as one
