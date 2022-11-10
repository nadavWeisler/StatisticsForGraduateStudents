# TA 2 - Confidence intervals, visualizations, assumption violation

# Gal Chen - galrefa.chen@mail.huji.ac.il
require(ggplot2)
require(dplyr)

### Simulating confidence interval
# First - how to? 
set.seed(25)
sample_a=rnorm(100,0,1)
df_a=length(sample_a)-1
se_a = sd(sample_a)/sqrt(1+df_a) # the sd function returns an already corrected (n-1) value
critical_t = qt(0.025,df=df_a) # finds the relevant t-score for a given percentile of the t-distribution
CI_95 =c(mean(sample_a)+critical_t*se_a,
         mean(sample_a)-critical_t*se_a) 
CI_95

# It's easier to draw the CI directly from the t.test
set.seed(25)
sample_a=rnorm(100,0,1)

t.test(sample_a) # What is the connection between the 95% CI and 
#                  the result of the t-test?
t.test(sample_a)$conf.int 
t.test(sample_a)$stderr 

# Another way is to use the "effectsize" package to get an estimation for cohen's d
effectsize::cohens_d(sample_a)

# let's simulate many repetition of the same experiment to look how the CIs behave:
sim_CI = function(mu=5,std=20,n=65){  # running should take a minute
  CI_df= data.frame(low_CI=c(),high_CI=c())
  for (i in 1:1000){
    curr_sample=rnorm(n,mean=mu,sd=std)
    curr_CI=t.test(curr_sample)$conf.int
    CI_df=rbind(CI_df,data.frame(low_CI=curr_CI[1],
                                 high_CI=curr_CI[2])) 
    # Each iteration, we add the mean of a random sample with n=100
  }
  return (CI_df)
}

CI_df=sim_CI()
#Let's try to answer this before running the code
# How many times was the parameter included in the sample?
mu=5
mean(CI_df$low_CI<mu & CI_df$high_CI>mu) 
# How many times was 0 included in the sample?
mean(CI_df$low_CI<0 & CI_df$high_CI>0)

CI_mini_df=CI_df[1:25,]
CI_mini_df=data.frame(X=c(CI_mini_df$low_CI,CI_mini_df$high_CI),number=rep(1:25,2),
                      CI=c(rep("Low",25),rep("High",25))) # Reformat for plot
CI_mini_df %>% 
  ggplot(aes(x=X, y= number)) +
  geom_vline(xintercept = 0,linetype="dashed",size=0.7)+
  geom_vline(xintercept = mu,linetype="dashed",size=0.7)+
  geom_line(aes(group = number))+
  geom_point(aes(color=CI), size=1.5) +  
  labs(y="Replication number")+xlim(-15.5,15.5)
# Why isn't the width of the CIs constant?

# I used the library ggplot2 for the last graph. I highly recommend going over ggplot and using it for future visualizations
# For another example, it can be used to create histograms and change their visualization for your needs:

CI_mini_df %>% 
  ggplot(aes(x=X, fill=CI))+ # definining a ggplot object and the aesthetics (in this case, the x-axis and the column by which colors are selected)
  geom_histogram(color="black",bins=25)+ # creating a histogram based on the aes() definitions above
  facet_grid(CI~.)+ # dividing the graphs to two graphs based on CI. try to check what happens without this line
  geom_vline(xintercept=mu,linetype="dashed")+ # add line for  marking the parameter
  ggtitle("Histograms of low and high limits of confidence intervals at 95%")+ # adding title to both plots
  scale_fill_brewer(palette = "Paired")+ #setting color pallette
  theme_classic() # Changing the theme for the graph to look nicely

# Good visualization is Crucial!!
# For help on using ggplot see:
# https://ggplot2.tidyverse.org/
# https://ggplot2-book.org/index.html
# Also, mastering the art of asking the right quesion on google/Stack Overflow can make your plotting life easier. It is a skill by itself.

# Check yourself by changing the arguments to sim_CI:
# - What happens to the CIs and to the rate of CIs including 0 as the sample size changes?
# - What happens to the CIs and to the rate of CIs including 0 as mu changes?
# - What happens to the CIs and to the rate of CIs including the parameter mu as it changes?

CI_df=sim_CI(mu=5,std=20,n=950)
#Let's try to answer this before running the code
# How many times was the parameter included in the sample?
mean(CI_df$low_CI<mu & CI_df$high_CI>mu)

# How many times was 0 included in the sample?
mean(CI_df$low_CI<0 & CI_df$high_CI>0)

CI_mini_df=CI_df[1:25,]
CI_mini_df=data.frame(X=c(CI_mini_df$low_CI,CI_mini_df$high_CI),number=rep(1:25,2),
                      CI=c(rep("Low",25),rep("High",25))) # Reformat for plot
CI_mini_df %>% 
  ggplot(aes(x=X, y= number)) +
  geom_vline(xintercept = 0,linetype="dashed",size=0.7)+
  geom_vline(xintercept = mu,linetype="dashed",size=0.7)+
  geom_line(aes(group = number))+
  geom_point(aes(color=CI), size=1.5) +  
  labs(y="Replication number")+xlim(-15.5,15.5)

## Violation of assumptions
# 1 - non-normal-data

sim_CI_exponent = function(mu=5,std=20,n=65){  # add noise from exponential distribution without changing the mean
  CI_df= data.frame(low_CI=c(),high_CI=c())
  for (i in 1:1000){
    exp_noise=5*rexp(n,2)  
    exp_noise=exp_noise-mean(exp_noise)
    curr_sample=rnorm(n,mean=mu,sd=std)+exp_noise
    curr_CI=t.test(curr_sample)$conf.int
    CI_df=rbind(CI_df,data.frame(low_CI=curr_CI[1],
                                 high_CI=curr_CI[2])) # Each iteration, we add the mean of a random sample with n=100
  }
  hist(curr_sample)
  return (CI_df)
}

CI_df=sim_CI(mu=5,std=20,n=100)
CI_df=sim_CI_exponent(mu=5,std=10,n=10) # Change n and see what happens (is 30 really enough?)

# Let's try to answer this before running the code:
# How many times was the parameter included in the sample?
mean(CI_df$low_CI<mu & CI_df$high_CI>mu)

# How many times was 0 included in the sample?
mean(CI_df$low_CI<0 & CI_df$high_CI>0)

CI_mini_df=CI_df[1:25,]
CI_mini_df=data.frame(X=c(CI_mini_df$low_CI,CI_mini_df$high_CI),number=rep(1:25,2),
                      CI=c(rep("Low",25),rep("High",25))) # Reformat for plot
CI_mini_df %>%
  ggplot(aes(x=X, y= number)) +
  geom_vline(xintercept = 0,linetype="dashed",size=0.7)+
  geom_vline(xintercept = mu,linetype="dashed",size=0.7)+
  geom_line(aes(group = number))+
  geom_point(aes(color=CI), size=1.5) +
  labs(y="Replication number")+xlim(-25.5,25.5)

# means are VERY noisy and this noise heavily depends on sample size

# violations of assumptions can sometims lead us to underestimate and over-estimate the power of our study, 
# and more importantly - cause us to wrongly estimate our p-values, increasing type I/II errors.
# Sample size can help dealing with violations but are sometimes not enough and it's hard to know when they are.
# However, in most cases - there are ways to deal with the violations by adjusting the fitted model to quantities/percentage/dependence in the data.
# Some of these methods will be mentioned in the course later.

# How can we look at normality?
# There are statistical tests, but (for reason I am skipping right now)
# they can sometimes lead to bad conclusions. I recommend looking at the data.
normal=rnorm(200)
hist(normal)
qqnorm(normal)
qqline(normal)

not_normal=rnorm(200)+runif(200,-3,3)
hist(not_normal)
qqnorm(not_normal)
qqline(not_normal)

very_not_normal=runif(200,-3,3)
hist(very_not_normal)
qqnorm(very_not_normal)
qqline(very_not_normal)


# Bonus AT HOME: 
#Another violation - dependence of samples

# sim_CI_dependence = function(mu=5,std=20,n=60){  # two measurements from each subject. the second measurement equals the first one+noise
#   CI_df= data.frame(low_CI=c(),high_CI=c())
#   for (i in 1:1000){
#     curr_sample=rnorm(n/2,mean=mu,sd=std)
#     curr_sample=c(curr_sample,curr_sample+rnorm(n/2,mean=0,sd=5))
#     curr_CI=t.test(curr_sample)$conf.int
#     CI_df=rbind(CI_df,data.frame(low_CI=curr_CI[1],
#                                  high_CI=curr_CI[2])) # Each iteration, we add the mean of a random sample with n=100
#   }
#   hist(curr_sample)
#   return (CI_df)
# }
# 
# # try only one of those each time
# CI_df=sim_CI(mu=5,std=10,n=10)
# CI_df=sim_CI_dependence(mu=5,std=10,n=10)
# #Let's try to answer this before running the code
# # How many times was the parameter included in the sample?
# mean(CI_df$low_CI<mu & CI_df$high_CI>mu)
# 
# # How many times was 0 included in the sample?
# mean(CI_df$low_CI<0 & CI_df$high_CI>0)
# 
# CI_mini_df=CI_df[1:25,]
# CI_mini_df=data.frame(X=c(CI_mini_df$low_CI,CI_mini_df$high_CI),number=rep(1:25,2),
#                       CI=c(rep("Low",25),rep("High",25))) # Reformat for plot
# CI_mini_df %>% 
#   ggplot(aes(x=X, y= number)) +
#   geom_vline(xintercept = 0,linetype="dashed",size=0.7)+
#   geom_vline(xintercept = mu,linetype="dashed",size=0.7)+
#   geom_line(aes(group = number))+
#   geom_point(aes(color=CI), size=1.5) +  
#   labs(y="Replication number")+xlim(-25.5,25.5)


# Bonus AT HOME: 
# percentage data (number of sucesses from 1-10/divided by 10) blocked between 0-1
# 
# sim_CI_percentage = function(mu=0.75,n=10){  # two measurements from each subject. the second measurement equals the first one+noise
#   CI_df= data.frame(low_CI=c(),high_CI=c())
#   for (i in 1:1000){
#     curr_sample=rbinom(n,10,mu)/10
#     curr_CI=t.test(curr_sample)$conf.int
#     CI_df=rbind(CI_df,data.frame(low_CI=curr_CI[1],
#                                  high_CI=curr_CI[2])) # Each iteration, we add the mean of a random sample with n=100
#   }
#   hist(curr_sample)
#   return (CI_df)
# }
# 
# mu=0.6 # success rate
# CI_df=sim_CI_percentage(n=100,mu)
# #Let's try to answer this before running the code
# # How many times was the parameter included in the sample?
# mean(CI_df$low_CI<mu & CI_df$high_CI>mu)
# 
# # How many times was 0.5 (chance level) included in the sample?
# mean(CI_df$low_CI<0.5 & CI_df$high_CI>0.5)
# 
# # How many times was 1 (maximum) below the high estimation?
# mean(CI_df$low_CI<1 & CI_df$high_CI>1)
# 
# 
# CI_mini_df=CI_df[1:25,]
# CI_mini_df=data.frame(X=c(CI_mini_df$low_CI,CI_mini_df$high_CI),number=rep(1:25,2),
#                       CI=c(rep("Low",25),rep("High",25))) # Reformat for plot
# CI_mini_df %>% 
#   ggplot(aes(x=X, y= number)) +
#   geom_vline(xintercept = 0.5,linetype="dashed",size=0.7)+
#   geom_vline(xintercept = mu,linetype="dashed",size=0.7)+
#   geom_line(aes(group = number))+
#   geom_point(aes(color=CI), size=1.5) +  
#   labs(y="Replication number")+xlim(0,1)
# 
# The violation becomes more severe as we approach the limit


