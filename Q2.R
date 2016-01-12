library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

facs <- cbind(as.data.frame(apply(training[,-9]
                                  , 2
                                  , Hmisc::cut2
                                  , g = 5
                                  , levels.mean = TRUE))
              , CompressiveStrength = training[,9]
              )
qplot(y = CompressiveStrength
      , x = as.numeric(row.names(facs))
      , colour = Age
      , data = facs)

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL <- select(training, starts_with("IL"))
preProc <- preProcess(IL, method = "pca", thresh = 0.8)
IL_pca <- predict(preProc, IL)
IL_pca2 <- prcomp(IL)


variances <- sort(preProc$std, decreasing = TRUE)^2
cumvariances <- cumsum(variances)/sum(variances)
cumvariances

stdevs <- sort(preProc$std, decreasing = TRUE)
cumsd <- cumsum(stdevs)/sum(stdevs)
cumsd

asis_model <- train(training$diagnosis ~.
              , data = IL
              , method = "glm")
pca_model <- train(training$diagnosis ~.
                   , data = IL_pca
                   , method = "glm")

asis_test <- confusionMatrix(testing$diagnosis, predict(asis_model, testing))

testing_transform <- predict(preProc, testing)
pca_test <- confusionMatrix(testing$diagnosis, predict(pca_model, testing_transform))
