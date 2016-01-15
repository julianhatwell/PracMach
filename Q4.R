#Q1
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

rnfr <- train(y~., data=vowel.train, method = "rf")
gboo <- train(y~., data=vowel.train, method = "gbm")

rnfr_p <- predict(rnfr, vowel.test)
gboo_p <- predict(gboo, vowel.test)

rnfr_c <- confusionMatrix(rnfr_p, vowel.test$y)
gboo_c <- confusionMatrix(gboo_p, vowel.test$y)
agac <- confusionMatrix(rnfr_p, gboo_p)

rnfr_c$overall[1]
gboo_c$overall[1]
agac$overall[1]

#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rnfr2 <- train(diagnosis~., data=training, method = "rf")
gboo2 <- train(diagnosis~., data=training, method = "gbm")
ldaa2 <- train(diagnosis~., data=training, method = "lda")

rnfr2_p <- predict(rnfr2, testing)
gboo2_p <- predict(gboo2, testing)
ldaa2_p <- predict(ldaa2, testing)

stck <- data.frame(rnfr2_p, gboo2_p, ldaa2_p, tst = testing[["diagnosis"]])
rnfr_stck <- train(tst~., data = stck, method = "rf")
pred_stck <- predict(rnfr_stck, testing$diagnosis)

cm_rnfr2 <- confusionMatrix(rnfr2_p, testing$diagnosis)
cm_gboo2 <- confusionMatrix(gboo2_p, testing$diagnosis)
cm_ldaa2 <- confusionMatrix(ldaa2_p, testing$diagnosis)
cm_pred_stck <- confusionMatrix(pred_stck, testing$diagnosis)

#Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

lass <- train(CompressiveStrength~., data = concrete, method = "lasso")

#4
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", "gaData.csv")
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)

visits.forecast = forecast(visits.exp.smoothing, nrow(testing))
# plot the forecast
plot(visits.forecast)

batty <- bats(tstrain)
batty.forecast <- forecast(batty, h = nrow(testing))
plot(batty.forecast)
lines(testing$date, testing$visitsTumblr)
inBounds <- (testing$visitsTumblr > fc$lower[,2] & 
               testing$visitsTumblr < fc$upper[,2])
mean(inBounds)

#5
library(caret)
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
q5 <- svm(CompressiveStrength~., data = training)
q5_p <- predict(q5, testing)
sqrt(mean((q5_p - testing$CompressiveStrength)^2))