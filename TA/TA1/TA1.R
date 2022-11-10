# TA 1 - Intro to R ,Reading data, basic manipulations, descriptives, sampling

# Gal Chen - galrefa.chen@mail.huji.ac.il

# In this course, we assume you already have a basic background in R. 
# However, we will go over the basics in the first TAs to ensure everyone is starting from similar places
# The level of the TAs will rise quickly! 
# If by the end of this tutorial you feel overwhelmed by R, you should allocate more time for practicing in the next two weeks.

# Why are we even doing R? Why not use something simpler?
# Multiple reasons: programming is a crucial life skill even for non-programmers, working directly with data, flexbility of options, unparalleled toolboxes.

#*****************************
#Before TA1 - please go over pre-TA and make sure you understand them.
#*****************************


## TA 1 - data frames, dplyr, read data, unique, table, ddply&summarise, select, subset, merge
setwd("C:/Users/galche/Dropbox/psych-ma-research-methods/ta-gal/ta1")
# data frames
# data frames are assortments of vectors(or columns). each vector in the data frame can be from a different class.
data.frame(name=("gal","jeff","stan","ahmed"),
           age=(19,20,45,18)) #ERROR! why?




#...




# To define a vector we need c(...)
my_first_df = data.frame(name=c("gal","daniela","stan","ahmed"),
                         age=c(19,20,45,18),
                         height=c(183,170,150,181),
                         is_female=as.logical(c(0,1,0,0)))
my_first_df
# we can use the write.csv function to save data frames
write.csv("my_first_df.csv",my_first_df) # ERROR! and a rather unclear one...
?write.csv # any function has documentation that allows us to understand how to use it.

write.csv(my_first_df,"my_first_df.csv")

my_first_df_reread = read.csv("my_first_df.csv")
my_first_df_reread

# we can access specific columns using $
my_first_df_reread$age
mean(my_first_df_reread$age)
rnorm(100,mean=11)
mean(rnorm(100,mean=11))

# when the dataframe is big
View(my_first_df_reread)


# reading a new data set from an experiment in which each subject heard 180 words, wrote what he heard and rated them on intelligibility and valence (1-7):
dat_words = read.csv("all_data_control.csv")
View(dat_words) # this is an example of how might a data coming out of an experiment looks like. Sad. Very sad.


# lets trim it a little:
require(dplyr) # libraries are a bunch of functions we load at once, allowing us to use them directly.
# if you don't have a library installed, try "install.packages("packagename")"

names(dat_words)
dat_words=select(dat_words,
       participant,age,trials.thisN,eng_name,category,
       frequency,log_freq,rms, syllables,
       valence_resp.keys,intel_resp.keys,intel_resp.rt,correct_guess)



# what can we learn regarding this data?
head(dat_words)
str(dat_words)

unique(dat_words$participant) #we have 180 rows from each subject
length(unique(dat_words$participant)) #34 pariticpants
table(dat_words$participant) #we have 180 rows from almost all subjects, three with 160
table(dat_words$category) #slightly more animals than object
table(table(dat_words$eng_name)) #most of the animals appeared 34 times, some of them 31
table(dat_words$eng_name,dat_words$participant) # a matrix that shows for each subject - which animals did s/he hear.
table(dat_words$correct_guess) #subjects are around 90% accuracy in identifying the word.

df_sum_correct = dat_words %>% group_by(participant) %>% summarise(correct_rate=mean(correct_guess))
# wait! whats that %>% thing?
# the "pipe" is a tool that helps to write code in a more readable way. A %>% function(c) is identical to calling function(a,c)
# especially useful when we make several operations at once, like above, otherwise we would write:
df_sum_correct =  summarise(group_by(dat_words,participant),correct_rate=mean(correct_guess))
df_sum_correct

# we usually want some summary of the data, and not examine single trials (BUT: later in the course we would analyze single trials)



exclude = df_sum_correct$participant[abs(scale(df_sum_correct$correct_rate))>3] #exclude outlier participants
df_sum_correct$participant %in% exclude # allows us to test, for each participant, whether s/he was excluded
c("a","b","c")[2]
c("a","b","c")[c(FALSE,TRUE,FALSE)] # Two different ways to index
df_sum_correct$correct_rate[df_sum_correct$participant %in% exclude]
hist(df_sum_correct$correct_rate,breaks=14)

# now lets subset the data to include only correct trials and without outlier paticipants:
dat_words_only_correct = subset(dat_words, !(participant %in% exclude)) # '!' sign is like writing 'not'
!(1+1==3)

df_word_correct_rate = dat_words_only_correct %>% group_by(eng_name) %>% summarise(correct_rate=mean(correct_guess))
head(df_word_correct_rate)

# now that we have the identification rate for each point, lets look only at the trials in which subjects were succesful in identifying the word
n_points=nrow(dat_words_only_correct)

dat_words_only_correct = subset(dat_words_only_correct, correct_guess)
n_deleted = n_points-length(dat_words_only_correct$trials.thisN)
print(paste("deleted",n_deleted,"points with wrong words"))

#now lets look at a summary by words:
df_sum_only_correct = dat_words_only_correct%>% group_by(word) %>% summarise(
  valence=mean(valence_resp.keys),
  log_freq=mean(log_freq), #this is not realy an average as thwe word have the same frequency every time it appears
  intell=mean(intel_resp.keys))
#ERROR! 


###
df_sum_only_correct = dat_words_only_correct %>% group_by(eng_name) %>% summarise(
  valence=mean(valence_resp.keys),
  log_freq=mean(log_freq), #this is not realy an average as the word have the same frequency every time it appears
  intell=mean(intel_resp.keys))
# AT HOME: try to add the word category into the columns. hint: category[1]
# Is there a difference in the intelligibility of animals and objects?


# now I want to add the correct rate for each word. how do I do that?
df_sum_only_correct=merge(df_sum_only_correct,df_word_correct_rate,by="eng_name")
(df_sum_only_correct)

# let's plot the relationships we see in the data:
plot(df_sum_only_correct$valence,df_sum_only_correct$intell)
plot(df_sum_only_correct$valence,df_sum_only_correct$correct_rate)
plot(df_sum_only_correct$intell,df_sum_only_correct$correct_rate) 

# summary statistics on valence
boxplot(df_sum_only_correct$valence)
hist(df_sum_only_correct$valence)
mean_val=mean(df_sum_only_correct$valence)
sd_val=sd(df_sum_only_correct$valence) # already corrected for n-1
se_val=sd_val/sqrt(length(df_sum_only_correct$eng_name)) # why write all this and not use numbers?
# or - How to get your paper retracted

#is the mean valence greater than 4?
t_score_from_4=(mean_val-4)/se_val #  AT HOME: try to use the t test function (start by ?t.test) yourself and check our computation. is the code correct?
t_score_from_4
1-pt(t_score_from_4,length(df_sum_only_correct$valence)-1) # is this a one-or two tailed p-value?

#####
# for loops and lapply

for(i in 1:10){
  print (i*2)
}

# an example would be removing the slow trials from each subject, but relative to the subjects individual RTs.
S=unique(dat_words_only_correct$participant)
dat_words_only_correct_noslow = data.frame() # create an empty dataframe
for (i in 1:length(S)){
  curr_dat=subset(dat_words_only_correct,participant==S[i]) #get subject i
  curr_dat$scale_rt=scale(curr_dat$intel_resp.rt) # scale RT
  n_trials_removed=sum(abs(curr_dat$scale_rt)>=3) #count the number of trials above/below z-score of 3
  print(paste(n_trials_removed,
              "trials removed for subject", S[i]))
  curr_dat=subset(curr_dat,abs(scale_rt)<3) # and remove those trials
  dat_words_only_correct_noslow=rbind(dat_words_only_correct_noslow,
                                      curr_dat) #return the updated dataset
  }
# AT HOME: add a column that, for each participant, counts the number of trials removed. what is the mean? 

# for loops are useful on many occasions, but sometimes (not always) they can be more elegant
remove_slow_trials= function(data,s,threshold){ #this function could be written somewhere else, so the code would be much more elegant and easy to read.
  curr_dat=subset(data,participant==s)
  curr_dat$scale_rt=scale(curr_dat$intel_resp.rt)
  n_trials_removed=sum(abs(curr_dat$scale_rt)>=threshold)
  print(paste(n_trials_removed,
              "trials removed for subject", S[i]))
  curr_dat=subset(curr_dat,abs(scale_rt)<threshold)
  return (curr_dat)
}



dat_words_only_correct_noslow = data.frame() # create an empty dataframe
for (i in 1:length(S)){
  dat_words_only_correct_noslow=rbind(dat_words_only_correct_noslow,
                                      remove_slow_trials(dat_words_only_correct,S[i],3))
  
}

