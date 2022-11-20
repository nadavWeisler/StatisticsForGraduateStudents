# TA 3 - power analysis, 1/2-way ANOVA

# Gal Chen - galrefa.chen@mail.huji.ac.il

### Power analysis
# There are two main reasons for conducting power analysis:
# 1 - practical: we want to be sure we are not running a sample which is too small 
#  (why not just add subjects? wait until the end)
# 2 - statistical - if the power is too low, 
#  the probability H0 is incorrect given p<0.05 is lower.

# Your result is not trustworthy if the power is too low - 
# if, for example, your power is 10% and alpha is 5%
# there is no real reason to think a significant result is very meaningful.

require(pwr) # The go-to package
require(pwr2ppl) #can make our lives easier for anova power analysis
require(ggplot2)
require(dplyr)
require(ggpubr)
require(effectsize)

# So you want to decide your sample size.
# A useful thing to do here is to understand how power changes as a function of sample size
power_ttest_loop_n_fun=function(mu=0,std=1,n=c(10,20,30,40)){
  critical_t = qt(0.95,df=n-1)
  power_df=data.frame(n=n,power=rep(NA, length(n)))
  for (curr_n in 1:length(n)){ # Choose a different sample size each time
    t_df= data.frame(t_score=c())
    for (i in 1:2000){ # perform 2000 t-tests with the current sample size
    curr_sample=rnorm(n[curr_n],mean=mu,sd=std) # Sample with the chosen settings
    curr_t=t.test(curr_sample)$statistic
    t_df=rbind(t_df,data.frame(t_score=curr_t)) 
  } # running should take a minute
  curr_power=mean(t_df>critical_t) # how many significant results?
  power_df$power[curr_n]=curr_power
  }
  return(power_df)
}

power_ttest_loop_n_fun() # without effect - type I error
power_ttest_loop_n_fun(0.2)
#lets plot the result for many options to learn about the relationship more closely:
power_df=power_ttest_loop_n_fun(0.5, n=seq(5,93,8)) 
power_df$d="0.5"
power_df_2=power_ttest_loop_n_fun(0.35, n=seq(5,93,8)) 
power_df_2$d="0.35"
rbind(power_df,power_df_2) %>% ggplot(aes(x=n,y=power,color=d)) +
  geom_line(size=1.5)+
  theme_classic()

# Simulation is important to understand the behind-the scenes of power analysis
#   and for simulation of more complex designs that cannot be solved analytically.
# But for the "classical" cases (when assumptions hold), R solves the problem for us.

pwr.t.test(n=21,d=0,type="one.sample",alternative="greater")
pwr.t.test(n=21,d=0.5,type="one.sample",alternative="greater")
pwr.t.test(n=21,d=0.35,type="one.sample",alternative="greater") # compare these results to results from above

# We can also choose the power we want, and find the desired sample size:
pwr.t.test(d=0.35,power=0.8,type="one.sample",alternative="greater")

# And rapidly get power estimates for different N's
pwr.t.test(n=c(10,20,30),d=0.35,type="one.sample",alternative="greater") 
powers=pwr.t.test(n=seq(5,93,2),d=0.35,type="one.sample",alternative="greater") # what will we get for d=0?

plot(powers$n,powers$power)
lines(powers$n,powers$power)



# But how do I know the effect size??
# Short answer - You don't. You make an educated guess, either based on previous study 
#  (yours or from another paper the did something close)
# or based on more general assumptions (i.e. 0.5 is considered a "medium effect size", 
#   the mean effect size in psychology is 0.4, etc..)

# The problem starts when our designs get more complex and we can't save them with analytic tools.
# A quick example is violation of assumptions - e.g. what happens to the power when our DV is uniformly distributed?
# In that case and many others, there is no choice but to use simulated power analyses.

############
#### 2-Way ANOVA
# We assume, as in all linear models, a normal distribution within groups (the residuals from the mean)

# Power analysis: the standard effect size in ANOVAs is explained variance, or eta-squared:
# It is the ratio between SSB/SST, or - the ratio of the variance between all samples,
#  that is explained by an effect.
# For power analysis, we need to calculate another effect size which is cohen f's 
#  (square root of SSB/SSW or sqrt(eta2/(1-eta2))) 
eta2=0.1
sqrt(eta2/(1-eta2))
f=eta2_to_f(eta2)
pwr.anova.test(3,30,f)


# For two-way anova:
set.seed(125)
sim_2way_anova = data.frame(dv=c(rnorm(30,7,2.3),rnorm(30,8.3,2.3),rnorm(30,8.7,2.3)),
                            group=c(rep("a",30),rep("b",30),rep("c",30)),
                                    gender=rep(c("M","F"),45))


sim_summary = sim_2way_anova %>% group_by(group,gender)%>% summarise(mean=mean(dv),std=sd(dv))
sim_summary # Summarizing the groups


# The full analysis should look like that:
aov_sim=aov(dv~group*gender,data=sim_2way_anova)
summary(aov_sim)
effectsize::eta_squared(aov_sim,partial=FALSE)

# But - pay we didn't check the normality assumption...
sim_2way_anova$residual=as.numeric(scale(aov_sim$residuals)) #turning all residuals (distances from group mean) into z-scores
par(mfrow=c(2,1))
hist(sim_2way_anova$residual,breaks=40) # there are few observations who wandered away from home...
qqnorm(sim_2way_anova$residual)
qqline(sim_2way_anova$residual)

sim_2way_anova=subset(sim_2way_anova,abs(residual)< 2.5) #removing outliers 
# (This qriteria MUST be decided before looking at the data)

par(mfrow=c(2,1))
hist(sim_2way_anova$residual,breaks=40) #looking better!
qqnorm(sim_2way_anova$residual)
qqline(sim_2way_anova$residual)


###
#Running again - without outliers
aov_sim=aov(dv~group*gender,data=sim_2way_anova)
summary(aov_sim) # it seems we have a significant interaction! 
# We would need to look at the simple effects to figure it's meaning.
effectsize::eta_squared(aov_sim,partial=FALSE)
# Based on the code, which of the effects reflect statistical errors? and what kind of error (I/II)?

# Plotting the groups
sim_2way_anova %>% ggline(x="group",y="dv",color="gender",add=c("mean_se","jitter")) 
# ggline is a "shortcut" function for some of the more popular ggplot applications.
# It belongs to the ggpubr package.

###
# In practice we know there is no difference between mean and women (How?)
# How does the inclusion of gender influences the results?
set.seed(12)
sim_2way_anova = data.frame(dv=c(rnorm(30,7.7,2.3),rnorm(30,8.3,2.3),rnorm(30,8.41,2.3)),
                            group=c(rep("a",30),rep("b",30),rep("c",30)),
                            gender=rep(c("M","F"),45))


# The full analysis should look like that:
aov_sim=aov(dv~group*gender,data=sim_2way_anova)
summary(aov_sim)
# In the case of one-way
aov_sim=(aov(dv~group,data=sim_2way_anova))
summary(aov_sim)
# We paid twice for adding an unnecessary factor - F value is smaller, and so was the residuals DF.

# Let's add an effect:
sim_2way_anova$dv[sim_2way_anova$gender=="F"]=sim_2way_anova$dv[sim_2way_anova$gender=="F"]+4
aov_sim=aov(dv~group*gender,data=sim_2way_anova)
summary(aov_sim)
aov_sim=aov(dv~group,data=sim_2way_anova)
summary(aov_sim)

# We also pay for not adding the factor when an effect existed. 
# A lot of variance was kept unexplained and added to the residuals.

# The solution - add ALL the factors you assume will have influence on the DV, and only them.

# More importantly, we need to decide about our factors ahead of time. 
# Otherwise (e.g., when we are running both the one and two way anova), we are inflating Type I error 
#  (another example of false positive psychology).


#################
#Extras:
# Code from class:
# let's simulate a t-test for samples drawn from H0 (population mean is zero)
# power_ttest_fun=function(mu=0,std=1,n=65){
#   critical_t = qt(0.95,df=n-1)
#   
#   t_df= data.frame(t_score=c())
#   for (i in 1:2000){
#     curr_sample=rnorm(n,mean=mu,sd=std)
#     curr_t=t.test(curr_sample)$statistic
#     t_df=rbind(t_df,data.frame(t_score=curr_t)) 
#   } # running should take a minute
#   
#   print(paste("Power:",mean(t_df>critical_t))) # how many significant results?
#   
#   t_df %>% 
#     ggplot(aes(x=t_score))+ # definining a ggplot object and the aesthetics (in this case, the x-axis and the column by which colors are selected)
#     geom_histogram(color="black",fill="darkgreen",bins=35)+ # creating a histogram based on the aes() definitions above
#     geom_vline(aes(xintercept=critical_t,color="Critical_t"))+ # add line for  marking the critical t
#     ggtitle(paste("Histograms of simulated t_scores with mu=",mu))+ # adding title to both plots
#     scale_color_manual(values = c(Critical_t = "red"))+
#     theme_classic() # Changing the theme for the graph to look nicely
# }
# 
# power_ttest_fun() # When there is no effect, this is simply the type I error
# power_ttest_fun(mu=0.3) # Let's add an effect
# power_ttest_fun(mu=15, std=50) # Same effect size, different mean
# power_ttest_fun(mu=0.3, n=25) # Same effect size, different n


# How to calculate F manually?

# SSB = sum(30*(sim_summary$mean-mean(sim_summary$mean))^2)
# MSB = SSB/(3-1) # df1=k-1
# # What are the chances to get these results under the null hypothesis mu1=mu2=mu3?
# aov_sim=(aov(dv~group,data=sim_1way_anova))
# hist(aov_sim$residuals) # as with t.test, aov() returns a list that contain a lot of valuable information about the analysis
# SSW=sum(aov_sim$residuals^2)
# MSW=SSW/(90-3) # df2=N-k
# F_value=MSB/MSW
# F_value
# p_value=1-pf(F_value,df1=3-1,df2=90-3) # 1 minus the chances to get the result we got, or smaller, under the F distribution.


# effect size
# The common effect size for between subject design is eta-squared, or - the percentage of explained variance.
#SST=sum((sim_1way_anova$dv-mean(sim_1way_anova$dv))^2)
#SST
#SSB/SST
