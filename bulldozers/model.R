#!/usr/bin/Rscript

## My model creates 6 different models, one for each ProductGroupDesc.  It uses GBM for
## each model with crossvalidation and some tuning parameters I obtained through trial
## and error.  It also only uses certain columns that I found to be useful by using
## various methods, such as random forest and gbm.

train <- read.csv("./data/processed.train.d.csv") #Data preprocessed using newpreprocess.R
test <- read.csv("./data/processed.test.d.csv")

set.seed(69)
library(caret)
library(gbm)

gbm.grid <- expand.grid(.interaction.depth=10,.n.trees=1000,.shrinkage=.1)
trctrl <- trainControl(method='cv', number=5)

# Make 6 different models, one for each of the unique ProductGroupDesc
# Only use entries with age > 0 and SaleYear > 2005
subsetsa<-list()
subsetsc<-list()
mod<-list()
for(i in 1:6){
  subsetsa[[i]]<-train[which(train$ProductGroupDesc.x==i & train$age>0 & train$SaleYear>2005),-c(2,4,6,7,9:13,18,27,30,38:40,45,47,50,51,52,54:57,59)]
  subsetsc[[i]]<-test[which(test$ProductGroupDesc.x==i),-c(1,3,5,6,8:12,17,26,29,37:39,44,46,49,50,51,53:56,58)]
  mod[[i]]<-train(SalePrice ~ ., subsetsa[[i]], method="gbm", trControl=trctrl, tuneGrid=gbm.grid)
  print(length(unique(subsetsa[[i]]$fiProductClassDesc.x)))
}

submit<-NULL
for(i in 1:6){
  guesses<-predict(mod[[i]],subsetsc[[i]])
  submit<-rbind(submit,cbind(exp(guesses),test[which(test$ProductGroupDesc.x==i),]$SalesID))
}

write.csv(submit,"submit.csv",row.names=F)