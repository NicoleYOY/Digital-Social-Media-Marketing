##  Codes for A/B test

#Suppose we have two versions of ads: A and B. We want to use A/B test to
#determine which version has a higher click through rate. First, we randomly assign our
#consumers into two groups: Group A (who receives ad A) and B (who
#receives ad B). Suppose the randomization is valid. Then, we create two
#dataset: one dataset ??ab_test_imp?? records the impression for each
#consumer in each group and the other dataset ??ab_test_goal?? records
#every click on the ad for each group (note that if a consumer did not click
# the ad, then this consumer is not recorded in this dataset).


# set the working directory and load the data

setwd("C:/Users/Nicole/Desktop/dataset")
ab.test.imp <- read.csv("ab_test_imp.csv",header=T, stringsAsFactors=F)
ab.test.goal <- read.csv("ab_test_goal.csv",header=T, stringsAsFactors=F)

# ab.test.imp contains all the users who were exposed to the ads.
#ab.test.goal contains all the users who 

head(ab.test.imp)
head(ab.test.goal)

str(ab.test.imp)
str(ab.test.goal)
# merge ab.test.imp and ab.test.goal

ab.test.com <- merge(ab.test.imp, ab.test.goal, by="transaction_id", all.x=T, suffixes=c("",".g"))

head(ab.test.com)

# 
# Add a column to record whether or not a user achieves a goal.

ab.test.com$is.goal <- ifelse(is.na(ab.test.com$user_id.g),0,1)
head(ab.test.com)

# 
# compute the purchase rate for each group
## one way to do this
subset_a=subset(ab.test.com,test_case=="A")
subset_b=subset(ab.test.com,test_case=="B")

click_rate_a=sum(subset_a$is.goal)/length(subset_a$is.goal)
click_rate_b=sum(subset_b$is.goal)/length(subset_b$is.goal)

t.test(subset_a$is.goal, subset_b$is.goal) # T test 

ab.result=data.frame(
test_case=c("A","B"),
cvr=c(click_rate_a,click_rate_b)
) # construct a data.frame for drawing the graph.

ab.result

#visualization

library(ggplot2)

ggplot(ab.result, aes(x = test_case, y=cvr)) + 
geom_bar(stat="identity")+geom_text(aes(label=test_case),vjust=-0.5)
 




