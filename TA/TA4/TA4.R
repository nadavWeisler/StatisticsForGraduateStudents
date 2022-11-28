#TA 4 Planned & post-hoc contrasts, multiple comparisons

require(dplyr)
require(emmeans)
# We will start today by replicating the data from the end of the last TA
# To make the matter understandable, let us assume the dv is now "rating" and the groups are positive/negative/neutral stimulus

set.seed(125)
sim_2way_anova = data.frame(rating=c(rnorm(30,7,2.3),
                                 rnorm(30,8.3,2.3),
                                 rnorm(30,8.7,2.3)),
                            group=as.factor(c(rep("negative",30),rep("netural",30),rep("positive",30))),
                            gender=rep(as.factor(c("M","F")),45))

# Pay attention to outliers...
aov_sim_with_outliers=(aov(rating~group*gender,data=sim_2way_anova))
sim_2way_anova$residual=as.numeric(scale(aov_sim_with_outliers$residuals)) #turning all residuals into z-scores
hist(sim_2way_anova$residual,breaks=40) # there are few observations who wandered away from home...
sim_2way_anova=subset(sim_2way_anova,abs(residual)< 2.5) #remving outliers (This qriteria MUST be decided before looking at the data)

aov_sim1way=(aov(rating~group,data=sim_2way_anova))
summary(aov_sim1way) 

# How do we determine where the group effect comes from?

emm=emmeans(aov_sim1way, specs = ~group) # table of means
emm


# If we have a-priori assumption (say, neutral&negative < positive), we will rely on planned contrasts:
levels(sim_2way_anova$group) # for knowing the order of the levels
contr_negneut_v_pos = c(0.5,0.5,-1) 
contrast(emm, method = list("Positive vs others"=contr_negneut_v_pos) ) 

#Bonus - manual calculation of contrasts
# a=sim_2way_anova %>% group_by(group) %>% summarise(m=mean(rating),n=length(rating))
# s=summary(aov_sim1way)
# msw=s[[1]]$`Mean Sq`[2]
# er=sqrt(msw*sum((contr_negneut_v_pos^2)/a$n))
# d=sum(contr_negneut_v_pos*a$m)
# d/er

# We can also add independent contrasts:
contr_neg_v_neut = c(1,-1,0)
sum(contr_neg_v_neut*contr_negneut_v_pos)
contrast(emm, method = list("Positive vs others"=contr_negneut_v_pos,
                            "Negative vs neutral"=contr_neg_v_neut)) 

# But the result from dependent contrasts is invalid(!):
contr_neg_v_pos = c(1,0,-1)
sum(contr_neg_v_pos*contr_negneut_v_pos) # not zero
contrast(emm, method = list("Positive vs others"=contr_negneut_v_pos,
                            "Negative vs positive"=contr_neg_v_pos)) 

# In 2-way anova
aov_sim2way=(aov(rating~group*gender,data=sim_2way_anova))
summary(aov_sim2way) # it seems we have a significant interaction! #where is it coming from?

# Plotting the groups
sim_2way_anova %>% ggline(x="group",y="rating",color="gender",add=c("mean_se","jitter"))+
  facet_wrap(~gender) # facet_wrap allows us to divide the graph into many parts by some factor
emm2=emmeans(aov_sim2way, specs = ~group*gender) # table of means
emm2

# But how do we know how to describe the meaning of the main effect and interaction
# We rely on contrasts to examine the simple effects.

# Situation 1: "Positive vs others"for both groups
# This time, the contrasts will be matrices, as we have more than 1 factor
contr_negneut_v_pos2 = c(.25,.25,-.5,.25,.25,-.5)
contrast(emm2, method = list("Positive vs others"=contr_negneut_v_pos2)) #group sizes are not identical, causing slight differences from 1-way contrast

# Situation 2: negative< positive&neutral for males but not for females
contr_negneut_v_pos2_m = c(0,0,0,-.5,.25,.25)
contr_negneut_v_pos2_f = c(-.5,.25,.25,0,0,0)
contrast(emm2, method = list("Negative vs others (men)"=contr_negneut_v_pos2_m,
                             "Negative vs others (women)"=contr_negneut_v_pos2_f)) 

# Situation 3: we a-priori hypothesize that men will rate the neutral stimuli higher, compared to women, but no such effect will appear for positive stimuli
emm2
contr_neut_m_v_f =  c(0,-1,0,0,1,0)
contr_pos_m_v_f =  c(0,0,-1,0,0,1)
contrast(emm2, method = list("Men vs Women (neutral)"=contr_neut_m_v_f,
                             "Men vs Women (positive)"=contr_pos_m_v_f))


# Okay cool. But what if we did not have a-priori hypothesis and just want insight into the differences?

#all simple effects between groups,within genders
emm <- emmeans(aov_sim2way, ~gender*group)
pairs(emm,adjust="bonferroni",simple = c("group")) 
pairs(emm,adjust="tukey",simple = c("group"))
pairs(emm,adjust="scheffe",simple = c("group"))

#all possible simple effects
emm <- emmeans(aov_sim2way, ~gender*group)
pairs(emm,adjust="bonferroni",simple = c("group","gender")) # why are there p-values of 1?
pairs(emm,adjust="tukey",simple = c("group","gender"))
pairs(emm,adjust="scheffe",simple = c("group","gender"))

# main effects
emm <- emmeans(aov_sim2way, ~group)
pairs(emm,adjust="bonferroni")
pairs(emm,adjust="tukey")
pairs(emm,adjust="scheffe")


# So it seems that for the main effects, we have evidence in favor of a general negative-positive difference 
# But why not use contrasts always? They are stronger....

# let us see what happens when we "blindly" rely on contrasts without a-priori hypothesis
contr_neg_v_pos =  c(-.5,0,.5,-.5,0,.5)
contr_neg_v_neut =  c(-.5,.5,0,-.5,.5,0)
contr_neut_v_pos = c(0,-.5,.5,0,-.5,.5)
sum(contr_neut_v_pos*contr_neg_v_neut)
sum(contr_neg_v_pos*contr_neg_v_neut)
sum(contr_neg_v_pos*contr_neut_v_pos) # all contrasts are dependent

contrast(emm2, method = list("Neutral vs negative"= contr_neg_v_neut,
                             "Positive vs negative"= contr_neg_v_pos,
                             "Positive vs neutral"= contr_neut_v_pos))
# let us remember the adjusted result:
pairs(emm,adjust="tukey")

# Note how different the p-values are!
# Remember - under the null hypothesis, we would like to recieve a "significant" result in one of 
#   the tests only 5% of the time
# This means that if we repeat the process above many times, the "bad" method may reveal itself 
#   by having a FA rate that is far from 5%

FA_rate_contrasts=c()
FA_rate_tukey=c()
for (i in 1:1000){
  set.seed(2*i)
  # everything we did before but a 1000 times
  sim_2way_anova = data.frame(rating=c(rnorm(30,8,2.3), #no main effect!
                                       rnorm(30,8,2.3),
                                       rnorm(30,8,2.3)),
                              group=as.factor(c(rep("negative",30),rep("netural",30),rep("positive",30))),
                              gender=rep(as.factor(c("M","F")),45))
  aov_sim_with_outliers=(aov(rating~group*gender,data=sim_2way_anova))
  sim_2way_anova$residual=as.numeric(scale(aov_sim_with_outliers$residuals)) #turning all residuals into z-scores
  sim_2way_anova=subset(sim_2way_anova,abs(residual)< 2.5) #removing outliers (This qriteria MUST be decided before looking at the data)
  aov_sim2way=(aov(rating~group*gender,data=sim_2way_anova))
  emm2=emmeans(aov_sim2way, specs = ~group*gender) # table of means
  df_contrast=data.frame(contrast(emm2, method = list("Neutral vs negative"= contr_neg_v_neut,
                               "Positive vs negative"= contr_neg_v_pos,
                               "Positive vs neutral"= contr_neut_v_pos)))
  
  emm = suppressMessages(emmeans(aov_sim2way, ~group))
  df_tukey=data.frame(pairs(emm,adjust="tukey"))
  
  #checking whether one of the methods returned (one or more) significant results by accident
  contrast_has_fa=sum(df_contrast$p.value<0.05)>0
  tukey_has_fa=sum(df_tukey$p.value<0.05)>0
  
  FA_rate_contrasts=c(FA_rate_contrasts,contrast_has_fa)
  FA_rate_tukey=c(FA_rate_tukey,tukey_has_fa)
  }
mean(FA_rate_contrasts)
mean(FA_rate_tukey)


# This could be even worse. If we decide,for example, whether to include/exclude outliers on the basis of their influence on the p-value.
# Another QRP would be adding 10 more subjects to each group if the effect is not significant

# Let's see what happens if we apply them both
FA_rate_outliers=c()
for (i in 1:1000){
  set.seed(i)
  sim_1way_anova = data.frame(rating=c(rnorm(60,8,2)),
                              gender=rep(as.factor(c("M","F")),30))
  
  aov_with_outliers=aov(rating~gender,data=sim_1way_anova)
  
  #without outliers
  sim_1way_anova$residual=as.numeric(scale(aov_with_outliers$residuals)) #turning all residuals into z-scores
  sim_1way_anova=subset(sim_1way_anova,abs(residual)< 2) #removing outliers
  aov_no_outliers=(aov(rating~gender,data=sim_1way_anova))
  
  #add 10 subjects to each group
  sim_1way_anova = rbind(sim_1way_anova[,1:2],data.frame(rating=c(rnorm(20,8,1)),
                                                         gender=rep(as.factor(c("M","F")),10)))
  aov_with_10more=aov(rating~gender,data=sim_1way_anova)
  
    
  #check for false alarms
    pvals=c(summary(aov_with_outliers)[[1]][1,5],
          summary(aov_no_outliers)[[1]][1,5],
          summary(aov_with_10more)[[1]][1,5]) #taking the p-values
  pvals_sig=sum(pvals<0.05)>0 # testing whether at least one of the test returned a significant result  

  FA_rate_outliers=c(FA_rate_outliers,pvals_sig)
}
mean(FA_rate_outliers) # and that's before even trying all the contrasts


