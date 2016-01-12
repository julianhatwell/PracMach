# PML Quiz 3
rm(list=ls())
library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(caret)
library(rattle)
library(pgmm)
library(randomForest)
# 1
data(segmentationOriginal)

training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]

set.seed(125)

segModel <- train(Class~.
                  , data = training
                  , method = "rpart")
print(segModel$finalModel)
fancyRpartPlot(segModel$finalModel)

# 3
data(olive)
olive <- olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

oliveModel <- train(Area~., data = olive, method = "rpart")
predict(oliveModel, newdata)

#4
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction) {
  sum(((prediction > 0.5)*1) != values)/length(values)
  }

set.seed(13234)
heartModel <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomial", data = trainSA)

trainingpredict <- predict(heartModel, trainSA)
testingpredict <- predict(heartModel, testSA)

missClass(trainSA$chd, trainingpredict)
missClass(testSA$chd, testingpredict)

# 5
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)
vowelModel <- train(y~., data = vowel.train, method = "rf")
order(varImp(vowelModel), decreasing = TRUE)

modelfit <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
order(varImp(modelfit), decreasing = TRUE)