#RF for Arthritis
library(dplyr)
Before113 <- read.csv("Before113n.csv", head=TRUE,dec = ",", sep=";", stringsAsFactors=FALSE)

#BoxCox data transformation
library(caret)
preBefore <-  preProcess(Before113, method = c("center", "scale","BoxCox"))
TransBefore = predict(preBefore, Before113)
TransBefore <- TransBefore %>% select(-IL22, -IL31, -IL33, -IL4, -BSA, -sPGA, -PASI)

training.samples_arth <- TransBefore$Arthritis%>%
  createDataPartition(p = 0.7, list = FALSE)
train.data_arth  <- TransBefore[training.samples_arth, ]
test.data_arth <- TransBefore[-training.samples_arth, ]

set.seed(123)
model_rf <- train(
  Arthritis ~., data = TransBefore[training.samples_arth, ], method = "rf",
  trControl = trainControl(method = "repeatedcv", 
                           number = 10, repeats = 3, summaryFunction = multiClassSummary, classProbs = TRUE), 
  importance = TRUE, 
)
model_rf$finalModel

predicted.classes_arth <- model_rf %>% predict(TransBefore[-training.samples_arth, ])
head(predicted.classes_arth)
RF_final = predict(model_rf, TransBefore[-training.samples_arth, ])
confusionMatrix(RF_final, TransBefore$Arthritis[-training.samples_arth])

#Compute model accuracy rate
mean(predicted.classes_arth == TransBefore$Arthritis)
model_rf$bestTune

#Variables importance measures can be plotted using the function varImpPlot() [randomForest package]
library(randomForest)
importance(model_rf$finalModel)
varImpPlot(model_rf$finalModel)
varImpPlot(model_arth$finalModel, type = 2)
#MeanDecreaseAccuracy, which is the average decrease of model accuracy in predicting the outcome of 
#the out-of-bag samples when a specific variable is excluded from the model.

#The function varImp() [in caret] displays the importance of variables in percentage:
varImp(model_rf)