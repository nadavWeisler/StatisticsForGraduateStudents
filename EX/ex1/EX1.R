# ?????? - 
# ???? ???, 209636885; ??? ??????, 316493758

########## Part 1 ##########

### 1. Download the dataset "attention-drives-emotion-2019-exp1" and load it in R. Read the methods section from the paper and the codebook attached. 

# setwd("C:/Users/????/Desktop/??????????/???? ???/????????? ?? ???? ??????/EX1")
attentionDrivesEmotion <- read.csv("attention-drives-emotion-2019-exp1.csv")

library("tidyverse")

### 2. Each line describes one rating. What are the relevant columns that describe the rated intensity of emotional response, rated emotional intensity, subject identity, valence of image rated and whether the image was a target or-non target?

# Find in the Word doc.

### 3. Create a new variable "rating" that is the average of rated emotional intensity and rated intensity of emotional response, for each image rated (hint: google ?how to average two columns??).

attentionDrivesEmotion$rating <- rowMeans(attentionDrivesEmotion[, c(14, 15)], na.rm = TRUE)

### 4. Subset the data set to remove:
# - Participants who answered they thought the experimenter wanted them to "to rate the target images as much more emotionally intense than the other images"
# - Trials with Neutral images

FilteredDF <- attentionDrivesEmotion %>% 
  filter(!(demand_intensity == 2)) %>% 
  filter(!(ValenceContrast == 0))


# How many participants were removed?
length(unique(attentionDrivesEmotion$id)) # Original number of participants
length(unique(FilteredDF$id)) # Filtered participants


### 5. Create a new variable "z_rating" which includes the z-scores of "rating" within subjects 
# (hint 1: use "group_by" as in the TA and then "mutate" instead of "summarise". The line ?mutate(data,new_column_name=old_column+1)? creates a copy of the previous data set with the new column added (in this example adding 1 to the previous column) ) 
# (hint 2: "scale" is the function for z-scoring a numeric vector).

DFWithZScores <- FilteredDF %>%
  mutate(z_rating = scale(rating)) %>% 
  ### 6. Use the variable to remove images which were rated +-2 SD from the subject average (within each subject). 
  filter((z_rating > -2) & (z_rating < 2))



### 7. Create a summary dataset which includes the following information for each subject and stimulus type (2 rows per subject, one for targets and one for non-targets):
# - mean rating
# - sd of rating
# - the number of trials left for each participant and stimulus type. 

DFSummary <- DFWithZScores %>%
  group_by(target) %>% 
  summarise

# What is the total number of target trials left? 


# You can use a ?for? loop for this and connect subsets of the data using ?rbind?, but this could also be performed through ?group_by? 
# (hint: you can use length(rating) to get the number of trials per subject&stimulus type after you group)



### 8. Load the gender data, which include the reported gender of each subject. Subjects reported their gender using text so they used different notation to write the same thing. Write a code to fix this dataset so that the "gender" column will include "F" for females, "M" for males and "O" for any other gender. 
# (hint: the "unique" function can assist you in finding all the different variations. You can use this line of code to find the relevant rows to edit for each variation: data[data$gender=="femal"]). How many females, males, and others are in the sample?


### 9. Merge the summary data with the gender data, so that another column "gender" will be added to the data. What are the means and standard deviations for each gender? Prefarably, report in a table.



### 10. Conduct the relevant t-test to test the authors' hypothesis using the t.test() function. 
# Assume normality and equal variances, and make sure you get all the arguments for the functions right (there are default settings which won?t necessarily match the kind of test you would like to conduct).
# Report the results verbally, as you would in a results section of a paper: means, standard error, t-score, degrees of freedom, and p-value. The reader should be able to understand from what you wrote whether the hypothesis was confirmed and what it means.




### 11. Plot a graph that summarize the result. You can use the "plot" or "hist" function from the base R library, or the function "ggpaired" from the "ggpubr" library. Feel free to use any function of your choice as long as the plot shows all the relevant data to understand the answer for the research question and for the conducted test.



########## Part 2 ##########

# The goal of this part is to perform a simulation. Simulations are an important tool to perturb our assumptions and find out the actual distributions we would expect to get. In this case, you will use simulations to look into the prevalence of Type I and Type II errors when using t-test under different conditions.

### 1.	Create a function named "t_test_sim(M1, M2, S1, S2, N1, N2)" ? the function gets as input two expected values and two standard deviations ? the parameters of two populations. It also receives two sample sizes for groups sampled from these populations. The function then (15 points):
# a. Samples two different groups from a normal distribution, with the parameters defined for the function as inputs. It performs an independent 2-sample t-test, assuming equal variances between them and save the output of the t.test function into another variable. The result should be a list summarizing the results of the test.
# b. Creates two new variables with the t-statistic and the p-value.
# c. Returns those two variables in a vector/dataframe.
# Call the function for conducting a t-test for two samples (with size 50) from the same population and report the t-score, degrees of freedom, and p-value.


### 2. Run a loop for 1,000 iterations with sample sizes of 30 on each group and equal standard deviations (unless instructed otherwise) (15 points ).
# In each iteration, call the function and save the output (t-scores and p-values) for simulations of the t-scores received when:
# a. The null hypothesis is correct (Cohen?s d=0, meaning the distance between population means is 0 sd)
# b. Cohen?s d=0 but one of the standard deviations is 3 times larger than the other (sd_a = 3*sd_b)
# c. Cohen?s d = 0 but sample size = 90 in each group
# d. Cohen?s d = 0.5
# e. Same means difference as (d) but one of the standard deviations is 3 times larger than the other (sd_a = 3*sd_b)
# f. Cohen?s d = 0.5 but sample size = 90 in each group
# Each time, visualize (as histograms) and save the distributions of p-values and t-scores you get. Make sure to give informative names to the variables and graphs you create!


### 3. Answer the following questions regarding the above distributions (15 points ):
# a. What was the p-value of the t-score you received in Part 1?
# b. What is the shape of the distribution of p-values when no effect is present (i.e., H0 is correct)? How does this distribution change when there a true effect is present (under H1)? 
# c. What happens to the distribution of p-values and t-scores when the sample size increases? How does it depend on the existence of an effect? Explain the results using the terms learned in class (e.g., Type I and Type II error, statistical power, sampling distribution).
# d. What happens to the distribution of p-values and t-scores when the variances are unequal? How does it depend on the existence of an effect? Explain the results using the terms learned in class (e.g., Type I and Type II error, statistical power, sampling distribution).


### 4. (15 points)
# a. Re-write the function you used for the simulations, this time setting the t.test() function to perform welch t-test (try to figure out how using ??t.test" ). How does the answer for question 3d changes when you do that? Explain in your times the influence of using welch t-test instead of regular t-test.
# b.Re-write the function to ask a question of your own using simulation on t-test. It could be the effect of any other kind of change in the sampling process. Make it interesting and be creative! Try to explain the result you got in your simulations.
