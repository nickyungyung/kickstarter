

# !diagnostics off

# Our overall structure would be: read the raw data, clean the dataset, then perform
# Exploratory Data Analysis and then clean the dataset again and do this process back and forth
# to get the most out of the raw dataset, the data set will then be split into training+validation
# and a test set.
# we would then fit our training data onto several models and improve their performance by 
# optimization on hyperparameter (if any). Different mdoels will be compare by their performance
# (precision, importance plot, ROC, etc) on the training set, then the final candidate will
# be chosen by their precision on the test set.


remove(list = ls())

library(readr)
library(anytime)
library(stringr)
library(randomForest)
library(rpart)
library(MASS)
# library(RJSONIO)
library(jsonlite)
library(rpart.plot)
library(RColorBrewer)
library(quantmod)


# library(caret)

# read data
kickstart.1 <- read_csv("D:/R/kickstart.csv")


# cleaning and EDA

# observe the dimension
dim(kickstart.1)


View(kickstart.1)

# library(GGally)
# 
# ggpairs(kickstart.1)


# by simplify examining the data with respect to our tasks, we observe that some variables need
# adjustments,

# there are 6 factor variables: state, disable_communication, country, currency, 
# currency trailing code, staff_pick

# there are also 3 numeric variables: goal, backers_count, static_usd_rate

# there are 6 variables encoded in JSON: photo, creator, location, category, profile,
# urls

# there are 4 variables related to time which is displayed in the UNIX format

# there are also 4 variables that are data-item identity related, 
# including: name, blurb, slug and id

# there is also a column for index and a column for urls entry

# and also 4 variables with unidentify entries: friends, is_starred, is_backing, permissions

# which leads us to a total of 10+3+6+4+4+2 = 29 variables.



# We would first set the factor variables as factors 
# In addition, noting that the "state" variable is the quantity that we are interested in,
# we adjust the levels as the questions intended, categories other than successful 
# are all classify as failure


# combine the levels of our interested variable
kickstart.1$state[kickstart.1$state != "successful"] <- "failure"

# change variables into factor
kickstart.1$state <- factor(kickstart.1$state)
kickstart.1$disable_communication <- factor(kickstart.1$disable_communication)
kickstart.1$country <- factor(kickstart.1$country)
kickstart.1$currency <- factor(kickstart.1$currency)
kickstart.1$currency_trailing_code <- factor(kickstart.1$currency_trailing_code)
kickstart.1$staff_pick <- factor(kickstart.1$staff_pick)

# next from our observation, for the four variables with unidentifiable entries,
# most of the entries are NA, only 11 (same data points with the four variables) of them 
# are meaningless quotation [] or FALSE. We could EITHER conclude that such variables
# are not helpful to our analysis and discard them, OR that those 11 entries have an 
# unobservable connection to each other and would prove helpful in predicting the "state".


# first note the strange and meaningless entries are in the same datapoint
# and we store those data point in sp
sp <- which(kickstart.1$friends != "NA")
sp

which(kickstart.1$is_starred != "NA")
which(kickstart.1$is_backing != "NA")
which(kickstart.1$permissions != "NA")

# check the have entry such as FALSE or []
(kickstart.1$permissions)[602]
(kickstart.1$is_backing)[602]
(kickstart.1$is_starred)[602]
kickstart.1$friends[602]


# We would carry out the latter approach avoiding the risk of losing information in the former
# approach
# By the following codes, we define a new variable "special", and classify the data with 
# entries in those four variables as class "2", and data without them as class "1",
# and remove the rest of the variable in our dataframe

kickstart.1["Special"] <- "1"
kickstart.1$Special[sp] <- "2"
kickstart.1$Special <- factor(kickstart.1$Special)
kickstart.1<- kickstart.1[,-c(26:29)]


# Moving on
# for "photo" variable, by reading it in JSON, we are able to extract multiple web links 
# (urls) that leads us to photos (png) that's very possibly related to the kickstarter item,
# ideally we could look for features in the picture that affects the output but that would be 
# a project in its own right, for example Google's Cloud vision API is a powerful tool in 
# recongizing features in photos, but we will skip it for now.

# read the web links by fromJSON
fromJSON(kickstart.1$photo[1])
kickstart.1 <- kickstart.1[,-3]


# the index of data point isn't paritcularly helpful as well, so we will remove them
kickstart.1 <- kickstart.1[,-1]

# the same goes for name, blurb and slug, we would need a completely different infrastucture
# to understand their effects on the output, their impact shall not be misregard but another
# piece of literature would be required

kickstart.1 <-kickstart.1[, -c(2,3,6)]

# nlp, bag of words, cda

# The "url" variable has (usually) included two component in their entry
# one would be a link to the start-up itself
# the other one, associated with "reward", is a link to the backing reward section of the
# start-up, it can be seen below, say for first data point


fromJSON(kickstart.1$urls[1])


# However, it is noted for the special data point "sp", they have quite a different
# entry, several urls are included which would point us to the log-in page of kickstarter
# and several links are in the deep web of kickstarter, leading us to believe that the 
# entries in the four variables are related to potential user preference hence might not be
# directly useful to our analysis but we will keep this structure in mind
fromJSON(kickstart.1$urls[sp[1]])




kickstart.1 <- kickstart.1[, -c(20)]


# one important observation is that the goal of the project depends on the currency 
# the project is using, we would need to change them to the same scale and we will be using
# USD for this project.
# we can see there are 13 levels in currency, we would need to deal with them in alphabetical order


summary(kickstart.1$currency)

from <- c("AUD","CAD", "CHF", "DKK", "EUR", "GBP", "HKD", "MXN", "NOK", "NZD", "SEK", "SGD")
to <- c("USD","USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD", "USD")

# we have extracted the exchange rate to ex.r
ex.r <- getQuote(paste0(from, to, "=X"))


kickstart.1$goal[which(kickstart.1$currency=="AUD")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="AUD")]*ex.r$Last[1]
kickstart.1$goal[which(kickstart.1$currency=="CAD")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="CAD")]*ex.r$Last[2]
kickstart.1$goal[which(kickstart.1$currency=="CHF")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="CHF")]*ex.r$Last[3]
kickstart.1$goal[which(kickstart.1$currency=="DKK")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="DKK")]*ex.r$Last[4]
kickstart.1$goal[which(kickstart.1$currency=="EUR")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="EUR")]*ex.r$Last[5]
kickstart.1$goal[which(kickstart.1$currency=="GBP")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="GBP")]*ex.r$Last[6]
kickstart.1$goal[which(kickstart.1$currency=="HKD")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="HKD")]*ex.r$Last[7]
kickstart.1$goal[which(kickstart.1$currency=="MXN")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="MXN")]*ex.r$Last[8]
kickstart.1$goal[which(kickstart.1$currency=="NOK")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="NOK")]*ex.r$Last[9]
kickstart.1$goal[which(kickstart.1$currency=="NZD")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="NZD")]*ex.r$Last[10]
kickstart.1$goal[which(kickstart.1$currency=="SEK")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="SEK")]*ex.r$Last[11]
kickstart.1$goal[which(kickstart.1$currency=="SGD")] <- 
  kickstart.1$goal[which(kickstart.1$currency=="SGD")]*ex.r$Last[12]



# We then change unix time variable into format that we could visually understand
kickstart.1$deadline <- anytime(kickstart.1$deadline)
kickstart.1$state_changed_at <- anytime(kickstart.1$state_changed_at)
kickstart.1$created_at <- anytime(kickstart.1$created_at)
kickstart.1$launched_at <- anytime(kickstart.1$launched_at)

# how to interpret the time data is not entirely straight forward, I would recommend breaking it
# down into several informative bits
# since directly giving the time as an input would deem unhelpful

# A different way of representing the system would be by the starting date
# and the duration
# The time difference of the launch time and deadline will be looked into
# the difference in create time and launch time as well
# the starting date (i.e year, month, weekday, time of the day)
# the state change time however is not so appropriate to our current situation, since we
# wouldn't know this input if we are given a set of information (i.e. a new startup program)
# using it in our prediction would yield data-snooping and unsensible interpretation.

# first calculate the time difference of launch and deadline and store it in "whole.t"
kickstart.1["up.t"] <- as.numeric(difftime(kickstart.1$deadline, kickstart.1$launched_at),
                                  c("days"))

# secondly calculate the time difference of create and launch
kickstart.1["before.t"] <- as.numeric(difftime(kickstart.1$launched_at, kickstart.1$created_at),
                                      c("days"))


# thirdly calculate time difference of create and deadline
kickstart.1["whole.t"] <- as.numeric(difftime(kickstart.1$deadline, kickstart.1$created_at),
                                     c("days"))

kickstart.1 <- kickstart.1[ ,-c(8:10)]

# the next step would be to extract information from the starting date, either the
# launch date or create day, starting with the launch date

kickstart.1$launched_at[1]

kickstart.1["s.year"] <- strftime(kickstart.1$launched_at, "%g")
kickstart.1["s.month"] <- strftime(kickstart.1$launched_at, "%B")
kickstart.1["s.date"] <- strftime(kickstart.1$launched_at, "%e")
kickstart.1["s.weekday"] <- strftime(kickstart.1$launched_at, "%A")
kickstart.1["s.hour"] <- strftime(kickstart.1$launched_at, "%H")
kickstart.1["s.minute"] <- strftime(kickstart.1$launched_at, "%M")

kickstart.1 <- kickstart.1[ ,-c(8)]

kickstart.1$s.year <- factor(kickstart.1$s.year)
kickstart.1$s.month <- factor(kickstart.1$s.month)
kickstart.1$s.date <- factor(kickstart.1$s.date)
kickstart.1$s.weekday <- factor(kickstart.1$s.weekday)
kickstart.1$s.hour <- factor(kickstart.1$s.hour)
kickstart.1$s.minute <- factor(kickstart.1$s.minute)



# we would also throw away the id column

kickstart.1 <- kickstart.1[ ,-c(1)]


# We then start to deal with the 5 variables with JSON entries

# starting with creator, we first extract the JSON component, and then check what categories
# exist, we observe that a total of 6 categories exist, including: avatar, id, is_registered,
# name, slug and urls
# where we observe that slug is not a component that every datapoint has, only 2880 possess it


dummy1 <- lapply(kickstart.1$creator, fromJSON)
dummy2 <- lapply(dummy1, attr, "names")
dummy3 <-factor(unlist(dummy2))
summary(dummy3)


# juptyer notebook

# from a randomly chosen datapoint 787 say, we observe that the avatar tab contains 3 subsessions
# which provide link to photos which we will discard this information as discussed above
# next the id contains the creator's id, the urls contains the link to the product website on
# kickstarter
# is_registered contains information whether that user is registered on kickstarter, and
# one easy check reveals that every creator (datapoint) is registered
# the name of the creator might contributes to the state, and on second glance some creators
# are deleted from the product, as written in name and potential have impact on state
# but for prediction purpose as describe in question,


all(lapply(dummy1, "[[", 2))


dummy4 <- lapply(dummy1, "[[", 3)
kickstart.1["creator.name"] <- unlist(dummy4)



# Assume the is the full data set that involves people included
# due to the large potential levels in creator.name
# we seperate the data set into people who have never created a project before and among 
# people who have created

# grouping people into created and never created may have unpredictable effect, they might fail
# their first try and want to try again, or they might have succeeded and confident that their
# idea may work from past experience
# hence we must keep their performance

length(which(duplicated(lapply(dummy1, "[[", 3))==TRUE))

kickstart.1$creator.name[-c(which(duplicated(dummy4)))] <- "first_attempt"

kickstart.1$creator.name[c(which(duplicated(dummy4)))] <- "no_first"

kickstart.1$creator.name <- factor(kickstart.1$creator.name)


# next location: country, displayable_name, id, is_root, name, short_name, slug, state, type
# urls


dummy12 <- lapply(na.exclude(kickstart.1$location), fromJSON)
dummy13 <- lapply(dummy12, attr, "names")
dummy14 <-factor(unlist(dummy13))
summary(dummy14)

# keep type of living accommodation
kickstart.1["living"] <- lapply(dummy12, "[[", 8)
kickstart.1$living <- factor(kickstart.1$living)


# next category

kickstart.1$category[1]
dummy21 <- lapply(kickstart.1$category, fromJSON)
dummy22 <- lapply(dummy21, attr, "names")
dummy23 <- factor(unlist(dummy22))
summary(dummy23)


dummy24 <- lapply(dummy21, "[[", 7)


kickstart.1["subcategory"] <- unlist(lapply(dummy21, "[[", 4))
kickstart.1["Category"] <- str_extract(dummy24, '^[^/]+' )

kickstart.1$Category <- factor(kickstart.1$Category)
kickstart.1$subcategory <- factor(kickstart.1$subcategory)


# next profile

kickstart.1$profile[1]
dummy31 <- lapply(kickstart.1$profile, fromJSON)
dummy32 <- lapply(dummy31, attr, "names")
dummy33 <- factor(unlist(dummy32))
summary(dummy33)

fromJSON(kickstart.1$profile[1])


# next urls

fromJSON(kickstart.1$urls[1])

kickstart.1 <- kickstart.1[, -c(10:14)]




kickstart.3 <- kickstart.1




View(kickstart.3)

kickstart.3 <- kickstart.3[, -c(7:8)]

ffds <- data.frame(model.matrix(~ subcategory, kickstart.3))
kickstart.3 <- cbind(kickstart.3, ffds)
kickstart.3 <- kickstart.3[, -c(19)]

kickstart.3 <- kickstart.3[, -c(17)]

kickstart.3 <- kickstart.3[, -c(5)]

summary(kickstart.3$subcategory)

kickstart.3$subcategory <- as.character(kickstart.3$subcategory)

kickstart.3$subcategory[kickstart.3$subcategory == "Children's Books"] <- "Childrens Books"
kickstart.3$subcategory[kickstart.3$subcategory == "Farmer's Markets"] <- "Farmers Markets"

kickstart.3$subcategory <- factor(kickstart.3$subcategory)

kickstart.4 <- kickstart.3



kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ state, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ disable_communication, kickstart.4)))

MM <- model.matrix(~ country - 1, kickstart.4)
MM <- MM[match(rownames(kickstart.4),rownames(MM)),]
kickstart.4 <- cbind(kickstart.4, MM)

kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ state, kickstart.4)))

kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ currency_trailing_code, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ Special, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ s.year, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ s.month, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ s.date, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ s.weekday, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ s.hour, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ creator.name, kickstart.4)))
kickstart.4 <- cbind(kickstart.4, data.frame(model.matrix(~ Category, kickstart.4)))



kickstart.4 <- kickstart.4[ , -c(2,3,4,5,7,11,12,13,14,15,16,17,18)]


#### 
# covariance matrix

pca <- prcomp(~ .,kickstart.4, na.action = na.omit)

loading <- as.data.frame(pca$rotation[,1:2])
which.max(loading[,1])

n <- length(loading[,2])
sort(loading[,2],partial=n-1)[n-1]

require(caret)
trans = preProcess(kickstart.4, na.action = na.omit)
PC = predict(trans, kickstart.4)

head(PC,3)

library(ggfortify)
autoplot(pca, data = na.omit(kickstart.4), color = 'state')

# # PCA
# 
# library(PCAmixdata)
# 
# as.numeric(kickstart.3[, c(1,8,9,11,12,13)])
# 
# pca.pr <- prcomp(pca[, c(1,8,9,11,12,13)])
# 
# 
# 
# pca <- as.matrix(as.data.frame(kickstart.3))
# 
# PCAmix(pca[, c(1,8,9,11,12,13)], pca[[, -c(1,8,9,11,12,13)]])
# 
# is.numeric(pca[, c(1)][1])
# as.numeric(pca[, c(1)]
# 
# library(FactoMineR)
# 
# 
# famd <- as.matrix(as.data.frame(kickstart.3))
# PCA(famd)


# split into test set 80:20
test.split <- sample(nrow(kickstart.1), 2000, replace = FALSE)

kickstart.2 <- kickstart.3[c(test.split), ] 
kickstart.3 <- kickstart.3[-(test.split),]



# classification tree

fit <- rpart(state ~ ., data=kickstart.3, method= "class")
summary(fit)


# resubstitution error rate (biased)
pred <- predict(fit, newdata = kickstart.3, type = "class")
mc <- table(kickstart.3$state, pred)

print(mc)
error.resub <- 1- (mc[1,1]+mc[2,2])/sum(mc)
print(error.resub)  # it is biased and underestimate

# test set

pred.1 <- predict(fit, newdata = kickstart.2, type = "class")
mc.1 <- table(kickstart.2$state, pred.1)

print(mc.1)
error.1 <- 1- (mc.1[1,1]+mc.1[2,2])/sum(mc.1)
print(error.1)  # it is biased and underestimate



# Cross-validation error

n <- nrow(kickstart.3)
K <- 10  # 10 fold cv
t <- n%/%K
set.seed(34)
a <- runif(n)
r <- rank(a)
b <- (r - 1)%/%t + 1
b <- as.factor(b)
print(summary(b))


all_error <- numeric(0)
for (k in 1:K){
  model <- rpart(state ~ ., data=kickstart.3[b!=k, ], method = "class")
  
  predi <- predict(model, newdata = kickstart.3[b==k, ], type = "class")
  
  mct <- table(kickstart.3$state[b==k], predi)
  
  err <- 1 - (mct[1,1] + mct[2,2])/sum(mct)
  
  all_error <- rbind(all_error, err)
}
err.cv <- mean(all_error)
print(err.cv)



# the one with the least cross validated error given by xerror
# tune hyperparameter such as cp and maxdepth
fit$cptable[which.min(fit$cptable[, "xerror"]),"CP"]

# underslash
fit.cp <- rpart(state ~ ., data=kickstart.3, method= "class", control = 
                  rpart.control(cp =fit$cptable[which.min(fit$cptable[, "xerror"]),"CP"]))

pred.2 <- predict(fit.cp, newdata = kickstart.2, type = "class")
mc.2 <- table(kickstart.2$state, pred.2)

print(mc.2)
error.2 <- 1- (mc.2[1,1]+mc.2[2,2])/sum(mc.2)
print(error.2)  # it is biased and underestimate


pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

rpart.plot(pfit, main= "Pruned")
rpart.plot(fit)

summary(fit.cp)


# # bootstrap fit
# 
# rgd <- sample(nrow(kickstart.1), 10000, replace = TRUE)
# kickstart.4 <- kickstart.1[rgd, ]
# 
# 
# boot.fit <- rpart(state ~ ., kickstart.4, control = rpart.control(xval = 10))
# 
# rpart.plot(boot.fit)
# rpart.plot(fit)
# 
# boot.fit$splits


# bagging
B <- 100
maxdepth <- 3

X <- kickstart.3[, -c(2)]


prediction_oob <- rep(0, length(kickstart.3$state))
numbertrees_oob <- rep(0, length(kickstart.3$state))

for (b in 1:B){

  subsample <- sample(1:n, n, replace =TRUE)
  outofbag <- (1:n)[-subsample]
  
  treeboot <- rpart(state ~ ., data = kickstart.3, subset = subsample, 
                    control = rpart.control(maxdepth = maxdepth))
  
  prediction_oob[outofbag] <- prediction_oob[outofbag] + 
    predict(treeboot, newdata = X[outofbag, ])
  
  numbertrees_oob[outofbag] <- numbertrees_oob[outofbag] +1
  
}

prediction_oob <- prediction_oob/numbertrees_oob
# if we have a set of data, we will put in our inputs then take result from
# all the different trees, then take the majority vote

# can compare the error rate




# randomforest
fit.rf <- randomForest(state ~ . - subcategory, 
                       kickstart.3, na.action = na.exclude)

# drop in GINI score
str(fit.rf)
print(fit.rf)
varImpPlot(fit.rf)
importance(fit.rf)


library(randomForest)
library(mlbench)
library(caret)

# there are a few hyperparameters that we can tune, mtry and ntree
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)


mtry <- sqrt(dim(kickstart.3)[2])
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(state~. - subcategory, data=na.exclude(kickstart.3), 
                    method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


# Random search 

# number = 10 equals 10 fold cv, and 3 repeats of the process to lower overfit

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(7)
mtry <- sqrt(dim(kickstart.3)[2])
rf_random <- train(state~., data=na.exlcude(kickstart.3), method="rf", 
                   metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


# grid search

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(state~., data=kickstart.3, method="rf", 
                       metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


# Algorithm tune

# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/



# out of bag error
plot(predict(fit.rf), kickstart.3$state)
pre.4 <- predict(fit.rf, newdata = kickstart.2)

mc.4 <- table(kickstart.2$state, pre.4)
print(mc.4)

error.oob.4 <- 1- (mc.4[1,1]+mc.4[2,2])/sum(mc.4)

print(error.oob.4)

library(inTrees)
treeList <- RF2List(fit.rf)


exec <- extractRules(treeList, kickstart.3[,-c(2)])
exec[1:2,]


# Measure rules. len is the number of variable-value pairs in a condition, freq is 
# the percentage of data satisfying a condition, pred is the outcome of a rule, i.e., 
# condition => pred, err is the error rate of a rule.
ruleMetric <- getRuleMetric(exec, kickstart.3[,-c(2)], kickstart.3$state)

ruleMetric[1:2,]

# prune the rules

summary((kickstart.3$subcategory))

ruleMetric <- pruneRule(ruleMetric, kickstart.3[,-c(2)], kickstart.3$state, maxDecay = 0.05)
ruleMetric[1:2,]


# pep8


# select compact rule set
(ruleMetric <- selectRuleRRF(ruleMetric, kickstart.3[,-c(2)], kickstart.3$state))

# The out-of-bag (oob) error estimate
# In random forests, there is no need for cross-validation or a separate test set to get 
# an unbiased estimate of the test set error. It is estimated internally, during the run, 
# as follows:
#   Each tree is constructed using a different bootstrap sample from the original data. 
#  About one-third of the cases are left out of the bootstrap sample and not used in the 
# construction of the kth tree.
# 
# Put each case left out in the construction of the kth tree down the kth tree to get a 
# classification. In this way, a test set classification is obtained for each case in about 
# one-third of the trees. At the end of the run, take j to be the class that got most of the 
# votes every time case n was oob. The proportion of times that j is not equal to the true
# class of n averaged over all cases is the oob error estimate. This has proven to be unbiased
# in many tests.





plotcp(fit)
print(fit)
plot(fit)
text(fit)
summary(fit)


str(fit.rf$predicted)
getTree(fit.rf)


# logistic regression
reg <- glm(state ~ ., data = kickstart.3, family = binomial)

summary(kickstart.3$living)

S = predict(reg, type = "response")
S

Y <- kickstart.3$state
S


roc.curve=function(s=0.5, print=FALSE){
   Ps=(S>s)*1
   FP=sum((Ps==1)*(Y=="failure"))/sum(Y=="failure")
   TP=sum((Ps==1)*(Y=="successful"))/sum(Y=="successful")
   if(print==TRUE){
   print(table(Observed=Y,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR", "TPR")
  return(vect)
}
threshold = 0.5
roc.curve(threshold, print=TRUE)

ROC.curve=Vectorize(roc.curve)

I=(((S>threshold)&(Y=="failure"))|((S<=threshold)&(Y=="successful")))

plot(S,Y,col=c("red","blue")[I+1],pch=19,cex=.7,,xlab="",ylab="")
abline(v=0.5,col="gray")

M.ROC=ROC.curve(seq(0,1,by=.01))
M.ROC
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l")

# Compare with classification tree
C <- predict(fit)
s = 0.5

Z <- kickstart.1$state


roc.curve.1=function(s=0.5, print=FALSE){
  Ds <- (C>s)[ ,1]
  Ds[Ds == TRUE] <- "failure"
  Ds[Ds == FALSE] <- "successful"
  FP=sum((Ds=="successful")*(Z=="failure"))/sum(Z=="failure")
  TP=sum((Ds=="successful")*(Z=="successful"))/sum(Z=="successful")
  if(print==TRUE){
    print(table(Observed=Z,Predicted=Ds))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR", "TPR")
  return(vect)
}

roc.curve.1(threshold,print=TRUE)

ROC.curve.1=Vectorize(roc.curve.1)

I.1=(((C>threshold)&(Z=="failure"))|((C<=threshold)&(Z=="successful")))

# plot(Ds,Z,col=c("red","blue")[I+1],pch=19,cex=.7,,xlab="",ylab="")
# abline(v=0.5,col="gray")

M.ROC.1=ROC.curve.1(seq(0,1,by=.01))
plot(M.ROC.1[1,], M.ROC.1[2,],col="grey", lwd=2 ,type="l")

plot(M.ROC[1,],M.ROC[2,],type="l")
lines(M.ROC.1[1,],M.ROC.1[2,],type="l",col="grey",lwd=2)

# library(animation)
# 
# saveGIF({
#   for (i in 1:10) plot(runif(10), ylim = 0:1)
# },loop=TRUE,interval=0.2)

# library(ROCR)
# pred_lr <- prediction(test, kickstart.1$state[as.numeric(attr(test, "names"))])
# perf <- performance(pred_lr, measure = "tpr", x.measure = "fpr")
# > plot(perf,col='red',lwd=2)
# > pred_lda <- prediction(proba_lda_test,Y[test])
# > perf <- performance(pred_lda, measure = "tpr", x.measure = "fpr")
# > plot(perf,col='blue',add=TRUE,lwd=2)
# > abline(a=0,b=1)
# > auc_lda <- as.numeric(performance(pred_lda,"auc")@y.values)
# > auc_lda
# > auc_lr <- as.numeric(performance(pred_lr,"auc")@y.values)
