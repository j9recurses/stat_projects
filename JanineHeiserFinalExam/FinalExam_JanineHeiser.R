#janine heiser
#final exam
#11-6-2013

#load data set 
datingdataset <- read.table("/home/j9/statsexam/Dating.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#examine the age variable
agez <- datingdataset$age
##check out the summary of the age variable
summary(agez)
sort(as.numeric(agez))
##sorted view shows 50+ people that 99 yrs old, which seems suspect
##since very few people live until 99. 
##Recode where age == 99 to NA. 
datingdataset$age[datingdataset$age == 99] <- "NA"
##check to see if the transform worked. 
sort(datingdataset$age)

##1b--> find mean age of twitter, vs those who don't
##mean who use twitter
summary(as.numeric(datingdataset$age[which(datingdataset$use_twitter =="Yes") ]))
##mean who don't 
summary(as.numeric(datingdataset$age[which(datingdataset$use_twitter =="No") ]))
##reload data
datingdataset <- read.table("/home/j9/statsexam/Dating.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

##just get the yes and no twitters
newdataset <- data.frame(age = datingdataset$age, twitter = datingdataset$use_twitter)
newdataset
newdataset <- newdataset[which(newdataset$age != 99),]

stuff <- unique(newdataset$twitter, incomparables = FALSE)
stuff
#turn the categories into numbers/factors
newdataset$twitternumb <- as.numeric(newdataset$twitter) - 1
newdataset

newdataset <- newdataset[which(newdataset$twitternum != 0),] 
newdataset

#code the binary variable, twitter
newdataset$bivar_twitter <- ifelse(newdataset$twitternumb == 4, "yes", "no")
newdataset

#conduct a t-test on the means
#null hypothesis: μ1 = μc 
#there is no difference in the mean age of those who use twitter and those who do not use twitter

# Check if the distribution of the age looks normal using a qqplot and qqline
#need to check this to meet assumption that variables are normally distributed within each group
pdf("/home/j9/statsexam/qqnormandline.pdf")
qqnorm(newdataset$age)
qqline(newdataset$age)
dev.off()
#qqnorm indicates that distribution is normal
#run the shapiro test to see if the means are normally distributed
##In statistics, the Shapiro–Wilk test tests the null hypothesis that a sample x1, ..., xn came from a normally distributed population
shapiro.test(newdataset$age)
##Small values of W are evidence of departure from normality and percentage points for the W statistic; here we have a large W value.
#the high value of W tells us that our data is normally distributed with a high level of confidence because the P-value is low too
#need to check assumption of Homogeneity of Variance, but we dont actually need to test this; 
# If variances are significantly different between groups, we should use a modified t-test that corrects for this problem (Welch’s t-test); welch's t-test is the #default for R
# The means of ageslook different between groups, twitter and no twitter
by(newdataset$age, newdataset$bivar_twitter, mean, na.rm = TRUE)
t.test(newdataset$age~ newdataset$bivar_twitter, newdataset)


#########
######Question 2########
###reload data for freshness
datingdataset_whole <- read.table("/home/j9/statsexam/Dating.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

##make a dataset
datingdataset <- data.frame(have_children = datingdataset_whole$have_children,  children12_17 = datingdataset_whole$children12_17, children0_5 = datingdataset_whole$children0_5, children6_11 = datingdataset_whole$children6_11)
datingdataset

#exam the variables for some suspicious
stem(datingdataset$children0_5)
sort(datingdataset$children0_5)
stem(datingdataset$children6_11)
sort(datingdataset$children6_11)
stem(datingdataset$children12_17)
sort(datingdataset$children12_17)

#Recode 99,98 values; nobody has 98 or 99 children
datingdataset$children0_5[datingdataset$children0_5 == 99] <- NA
datingdataset$children0_5[datingdataset$children0_5 == 98] <- NA
datingdataset$children6_11[datingdataset$children6_11 == 99] <- NA
datingdataset$children6_11[datingdataset$children6_11 == 98] <- NA
datingdataset$children12_17[datingdataset$children12_17 == 99] <- NA
datingdataset$children12_17[datingdataset$children12_17 == 98] <- NA

#check to make sure replacements work
sort(datingdataset$children0_5)
sort(datingdataset$children6_11)
sort(datingdataset$children12_17)

##sum up all the children by row
#make a data frame so we can add up values
newdataset <- data.frame(children12_17 = datingdataset$children12_17, children0_5 = datingdataset$children0_5, children6_11 = datingdataset$children6_11)
datingdataset$allchildren <- rowSums( newdataset , na.rm = TRUE) 
datingdataset
stem(datingdataset$allchildren)

#Looking for missing variables; recall that we replaced 98 and 99. Do any of these people say they have children, but then have zero children listed in the allchildren field? 
cool <- datingdataset[which( datingdataset$allchildren == 0),] 
cool
cool2 <- cool[which( cool$have_children == "Yes"),] 
cool2

##Found 9 instances like this that have this problem. Lets reload the data, this time not recoded. 
datingdataset2 <- data.frame(have_children = datingdataset_whole$have_children,  children12_17 = datingdataset_whole$children12_17, children0_5 = datingdataset_whole$children0_5, children6_11 = datingdataset_whole$children6_11)
datingdataset2
#make a data frame so we can add up values
newdataset2 <- data.frame(children12_17 = datingdataset2$children12_17, children0_5 = datingdataset2$children0_5, children6_11 = datingdataset2$children6_11)
datingdataset2$allchildren <- rowSums( newdataset2 , na.rm = TRUE) 
datingdataset2
cool <- datingdataset2[which( datingdataset2$allchildren > 50) ,] 
cool
cool2 <- cool[which( cool$have_children == "Yes"),] 
cool2

#A value that you don't see in anywhere in the number of children variables is the value of 8; I would expect to see that someone would have 8 children
somewhere in this data set; its a lot of kids but not also unheared of. So, I'm thinking that the value of 98 should be probably be 8.

#Recreate the data frame again to replace 98 with 8.
datingdataset3 <- data.frame(have_children = datingdataset_whole$have_children,  children12_17 = datingdataset_whole$children12_17, children0_5 = datingdataset_whole$children0_5, children6_11 = datingdataset_whole$children6_11)
datingdataset3

#Recode 99,98 values; nobody has 98 or 99 children
datingdataset3$children0_5[datingdataset3$children0_5 == 99] <- NA
datingdataset3$children0_5[datingdataset3$children0_5 == 98] <- 8
datingdataset3$children6_11[datingdataset3$children6_11 == 99] <- NA
datingdataset3$children6_11[datingdataset3$children6_11 == 98] <- 8
datingdataset3$children12_17[datingdataset3$children12_17 == 99] <- NA
datingdataset3$children12_17[datingdataset3$children12_17 == 98] <- 8

##sum up all the children by row
#make a data frame so we can add up values
newdataset3 <- data.frame(children12_17 = datingdataset3$children12_17, children0_5 = datingdataset3$children0_5, children6_11 = datingdataset3$children6_11)
datingdataset3$allchildren <- rowSums( newdataset3 , na.rm = TRUE) 
datingdataset3
sort(datingdataset3$allchildren)
stem(datingdataset3$allchildren)

##prt c, choosing a categorical variable, region.   
datingdataset4 <- data.frame(region = datingdataset_whole$region, have_children = datingdataset_whole$have_children,  children12_17 = datingdataset_whole$children12_17, children0_5 = datingdataset_whole$children0_5, children6_11 = datingdataset_whole$children6_11)
datingdataset4
#Recode 99,98 values; nobody has 98 or 99 children
datingdataset4$children0_5[datingdataset4$children0_5 == 99] <- NA
datingdataset4$children0_5[datingdataset4$children0_5 == 98] <- 8
datingdataset4$children6_11[datingdataset4$children6_11 == 99] <- NA
datingdataset4$children6_11[datingdataset4$children6_11 == 98] <- 8
datingdataset4$children12_17[datingdataset4$children12_17 == 99] <- NA
datingdataset4$children12_17[datingdataset4$children12_17 == 98] <- 8
##sum up all the children by row
#make a data frame so we can add up values
newdataset4 <- data.frame(children12_17 = datingdataset4$children12_17, children0_5 = datingdataset4$children0_5, children6_11 = datingdataset4$children6_11)
datingdataset4$allchildren <- rowSums( newdataset4 , na.rm = TRUE) 
datingdataset4

# look at the means, by each categories
by(datingdataset4$allchildren, datingdataset4$region, mean, na.rm=T)

##now use levine's test to check for homogenity of variance
require(car)
leveneTest(datingdataset4$allchildren, datingdataset4$region)

##now use a shapiro test to test for normal distribution
shapiro.test(datingdataset4$allchildren)

##now, lets run the anova, since we met the assumptions

# Perform the analysis of variance and check the significance
aovm = aov(allchildren ~ region, datingdataset4)
summary(aovm)

################
####Question 3 Regression
###reload data for freshness
datingdataset_whole <- read.table("/home/j9/statsexam/Dating.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

##make a dataframe
datingdataset <- data.frame(qlife = as.numeric(datingdataset_whole$life_quality),  yrsinr = datingdataset_whole$years_in_relationship)
datingdataset

#check to see if qlife is numeric
is.numeric(datingdataset$qlife)
is.numeric(datingdataset$yrsinr)

#recode to numeric
datingdataset$yrsinr <- as.numeric(datingdataset$yrsinr)

exam variable
stem(datingdataset$qlife)
sort(datingdataset$qlife)

#look at the values 
stuff <- unique(datingdataset$qlife, incomparables = FALSE)
stuff

##recode qlife variable 
##question says that variable is on scale of 1-5 get rid of 6 7 values
datingdataset$qlife_recoded <- as.numeric(recode(datingdataset$qlife, "1=5; 2=4; 3=3; 4=2 ; 5=1; 6=NA; 7=NA"))
datingdataset$qlife_recoded
is.numeric(datingdataset$qlife_recoded)

##find mean quality of life 
mean(datingdataset$qlife_recoded, na.rm=T)

##now lets start looking doing an OLS regression. 

##first, lets peek the scatter plot
pdf("/home/j9/statsexam/scatterplotq3b.pdf")
scatterplot(datingdataset$qlife_recoded, datingdataset$yrsinr)
dev.off()

library(car)

##now lets model this
model = lm( qlife_recoded ~ yrsinr, data = datingdataset)

pdf("/home/j9/statsexam/plotqz.pdf")
# use the plot command for common regression diagnostics
plot(model)
dev.off()

# plot 1: residuals versus fitted values.  check for heteroskedasticity.  Also, can look for non-linear relationships.
# plot 2: qqplot of standardized residuals vs. normal curve.  Check to see if the errors are normally distributed
# plot 3: scale-location.  variance of the residuals appears as points higher on y-axis
# plot 4: residuals vs leverage. Look for outliers that may be biasing the model

# look for points with large residuals outlierTest(modelThe residuals vs fitted values plot looks heteroskedastic, we can check with a Breusch-Pagan test.
library(lmtest)
bptest(model)

# use the plot command for common regression diagnostics
plot(model)

# plot 1: residuals versus fitted values.  check for heteroskedasticity.  Also, can look for non-linear relationships.
# plot 2: qqplot of standardized residuals vs. normal curve.  Check to see if the errors are normally distributed
# plot 3: scale-location.  variance of the residuals appears as points higher on y-axis
# plot 4: residuals vs leverage. Look for outliers that may be biasing the model

# look for points with large residuals 
outlierTest(model)

# The residuals vs fitted values plot looks heteroskedastic, we can check with a Breusch-Pagan test.
library(lmtest)
bptest(model)

##now look at the summary of the model
summary(model, cor=FALSE)
attributes(model)
##find the slope: 
model$coefficients[2]
##y intercept
model$coefficients[1]

#part c:
##make a dataframe
datingdataset <- data.frame(qlife = as.numeric(datingdataset_whole$life_quality),  yrsinr = datingdataset_whole$years_in_relationship, useinternet = as.numeric(datingdataset_whole$use_internet))
datingdataset

#check to see if qlife is numeric
is.numeric(datingdataset$qlife)
is.numeric(datingdataset$yrsinr)

#recode to numeric
datingdataset$yrsinr <- as.numeric(datingdataset$yrsinr)

exam variable
stem(datingdataset$qlife)
sort(datingdataset$qlife)

#look at the values 
stuff <- unique(datingdataset$qlife, incomparables = FALSE)
stuff

##recode qlife variable 
##question says that variable is on scale of 1-5 get rid of 6 7 values
datingdataset$qlife_recoded <- as.numeric(recode(datingdataset$qlife, "1=5; 2=4; 3=3; 4=2 ; 5=1; 6=NA; 7=NA"))
datingdataset$qlife_recoded
is.numeric(datingdataset$qlife_recoded)

#make a model now with internet usage. 
model4 = lm( qlife_recoded ~ yrsinr, useinternet, data = datingdataset)
summary(model4, cor=FALSE)

