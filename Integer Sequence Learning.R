
install.packages("AUC")
library(stringr)
library(plyr)
library(AUC)

# clear workspace
rm(list=ls())

# set base directory
base.dir <- "C:/Users/Ta Onisuka San/Documents/My R/Predict OEIS"
setwd(base.dir)

# load training dataset
train <- read.csv("train.csv",stringsAsFactors=FALSE)
test <- read.csv("test.csv",stringsAsFactors=FALSE)

sequences <- strsplit(train$Sequence, split=",")

# find the last element of each sequence 
finalElements <- sapply(sequences, tail, n=1)
frequencies <- table(finalElements)
index <- which.max(frequencies)
mostFrequentFinalElement <- names(frequencies)[index]
mostFrequentFinalElement


# set the seed and sample dataset
set.seed(123)
trainingIndices <- c(
  sample(which(finalElements==mostFrequentFinalElement),
         size=sum(finalElements==mostFrequentFinalElement)%/%2,
         replace=FALSE),
  sample(which(finalElements!=mostFrequentFinalElement),
         size=sum(finalElements!=mostFrequentFinalElement)%/%2,
         replace=FALSE))


trainingIndices_1 <- c(
  sample(which(finalElements==mostFrequentFinalElement),
         size=sum(finalElements==mostFrequentFinalElement)%/%2,
         replace=FALSE))

testIndices <- setdiff(1:length(sequences), trainingIndices)
str(trainingIndices)


# first step is to engineer some features, count the number of 1s before the last element
features <- data.frame(
  previousCount=sapply(sequences[trainingIndices],
                       function(sequence)
                       {
                         sum(head(sequence, n=-1)==mostFrequentFinalElement)
                       })
)

# predictive analysis
maxCount <- max(features$previousCount)
classProbabilities <- sapply(0:maxCount,
                             function(count)
                             {
                               mean(finalElements[trainingIndices][features$previousCount==count]==mostFrequentFinalElement)
                             })

plot(x=0:maxCount,
     y=classProbabilities,
     ylab=paste0("Probability Final Element is ",mostFrequentFinalElement),
     xlab=paste0("Number of ",mostFrequentFinalElement,"s in the Sequence"))

# get max value before final element
features$max <- sapply(sequences[trainingIndices],
                       function(sequence)
                       {
                         max(as.numeric(head(sequence,-1)))
                       })

# create a bucket
buckets <- cut(features$max, quantile(features$max, seq(0,1,by=0.1)))
classProbabilities <- sapply(split(finalElements[trainingIndices],buckets),
                             function(finalElementsByBucket)
                             {
                               mean(finalElementsByBucket==mostFrequentFinalElement)
                             })

par(mar=c(12,4,4,2)+0.1)
plot(x=1:10,
     xaxt="n",
     y=classProbabilities,
     ylab=paste0("Probability Final Element is ",mostFrequentFinalElement),
     xlab="",las=2)

axis(side=1,at=1:10,labels=levels(buckets),las=2)
title(xlab=paste0("Sequence Maximum (Bucketed)"),line=10)

# correlation with negatives
features$anyNegatives <- sapply(sequences[trainingIndices],
                                function(sequence)
                                {
                                  any(grepl("-",head(sequence,-1)))
                                })

classProbabilities <- c(mean(finalElements[trainingIndices][features$anyNegatives]==mostFrequentFinalElement),
                        mean(finalElements[trainingIndices][!features$anyNegatives]==mostFrequentFinalElement))

plot(x=1:2,
     y=classProbabilities,
     ylab=paste0("Probability Final Element is ",mostFrequentFinalElement),
     xlab="Is There a Negative Value in the Sequence?",
     xaxt="n")
axis(side=1,at=1:2,labels=c("Yes","No"))

# fit logistic regression
features$maxBucketed <- "13 or less"
features$maxBucketed[features$max>=14 & features$max<=115] <- "14 - 115"
features$maxBucketed[features$max>=116 & features$max<=512] <- "116 - 512"
features$maxBucketed[features$max>=513] <- "513 or greater"

# define the levels manually so we capture the ordering
features$maxBucketed <- factor(features$maxBucketed,
                               levels=c("13 or less",
                                        "14 - 115",
                                        "116 - 512",
                                        "513 or greater"))

features$class <- finalElements[trainingIndices]==mostFrequentFinalElement

# apply general linear model to train dataset
glm.fit <- glm(class ~ previousCount 
                     + maxBucketed 
                     + anyNegatives, family="binomial",data=features)
summary(glm.fit)

# feature engineering TEST dataset
features <- data.frame(
  previousCount=sapply(sequences[testIndices],
                       function(sequence)
                       {
                         sum(head(sequence,-1)==mostFrequentFinalElement)
                       }),
  anyNegatives=sapply(sequences[testIndices],
                      function(sequence)
                      {
                        any(grepl("-",head(sequence,-1)))
                      }),
  max=sapply(sequences[testIndices],
             function(sequence)
             {
               max(as.numeric(head(sequence,-1)))
             }),
  class=finalElements[testIndices]==mostFrequentFinalElement
)

features$maxBucketed <- "13 or less"
features$maxBucketed[features$max>=14 & features$max<=115] <- "14 - 115"
features$maxBucketed[features$max>=116 & features$max<=512] <- "116 - 512"
features$maxBucketed[features$max>=513] <- "513 or greater"

# define the levels manually so we capture the ordering
features$maxBucketed <- factor(features$maxBucketed,
                               levels=c("13 or less",
                                        "14 - 115",
                                        "116 - 512",
                                        "513 or greater"))

predictions <- predict(glm.fit,features,type="response")

auc(roc(predictions,factor(features$class)))

# predict integer sequences on test dataset
test.seq <- strsplit(test$Sequence, split=",")

test.features <- data.frame(
  previousCount=sapply(test.seq,
                       function(test.seq)
                       {
                         sum(head(test.seq,-1)==mostFrequentFinalElement) +
                         sum(tail(test.seq, 1)==mostFrequentFinalElement)
                       }),
  anyNegatives=sapply(test.seq,
                      function(test.seq)
                      {
                        any(grepl("-",head(test.seq,-1))) ||
                        any(grepl("-",tail(test.seq, 1))) 
                      }),
  max=sapply(test.seq,
             function(test.seq)
             {
               max(test.seq)
             })
)

# define max bucket

test.features$maxBucketed <- "13 or less"
test.features$maxBucketed[features$max>=14 & test.features$max<=115] <- "14 - 115"
test.features$maxBucketed[features$max>=116 & test.features$max<=512] <- "116 - 512"
test.features$maxBucketed[features$max>=513] <- "513 or greater"


#predict
predict.test <- predict(glm.fit, test.features, type = 'response')
submit <- data.frame(Id = test$Id, 
                     One_N = test.features$previousCount,
                     anyNegative = test.features$anyNegatives, 
                     maxBucket = test.features$maxBucketed,
                     score = predict.test)

submit$Last <- ifelse(submit$score > 0.9, 1, 0)

keep  <- c("Id", "Last")
submit <- submit[keep]
write.csv(submit, file = "integer_seq.csv", row.names = FALSE)







