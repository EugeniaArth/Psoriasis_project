#Процедуры сравнения эффективности моделей классификации
#Проводим разделение имеющейся выборки на обучающую и проверочную (использование фактора в качестве 
#аргумента функции createDataPartition() обеспечивает необходимый баланс его уровней при разделении) глава6.5. 

Before113 <- read.csv("Before113n.csv", head=TRUE,dec = ",", sep=";", stringsAsFactors=FALSE)
#BoxCox data transformation
preBefore <-  preProcess(Before113, method = c("center", "scale","BoxCox"))
TransBefore = predict(preBefore, Before113)

TransBefore$Arthritis <- as.factor(TransBefore$Arthritis)

install.packages("mlbench")
library(mlbench)
TransBefore$Arthritis <- as.factor(TransBefore$Arthritis)
#Для обучения выделили 70% (115 элементов) исходной выборки:
set.seed(7)
train <- unlist(createDataPartition(TransBefore$Arthritis , p = 0.7)) 
#определение схемы тестирования
control <- trainControl(method = "repeatedcv", 
                        number = 10, repeats = 3, summaryFunction = multiClassSummary, classProbs = TRUE)
#Выполняем обучение 
# CART - дерево классификации
install.packages("MLmetrics")
library(MLmetrics)
set.seed(7)
fit.cart <- train(TransBefore[train, 2:18], TransBefore$Arthritis[train],
                  method = "rpart", tuneLength = 50, trControl = control)
fit.cart      
CART = predict(fit.cart, TransBefore[-train, 2:18])
confusionMatrix(CART, TransBefore$Arthritis[-train])
# RF - случайный лес
set.seed(7)
fit.rf <- train(TransBefore[train, 2:18], TransBefore$Arthritis[train],
                method = "rf", trControl  = control)
fit.rf
RF = predict(fit.rf, TransBefore[-train, 2:18])
confusionMatrix(RF, TransBefore$Arthritis[-train])
# GLM - биноминальная логистическая регрессия
set.seed(7)
fit.glm <- train(TransBefore[train, 2:18], TransBefore$Arthritis[train],
                 method = "glm", family = binomial, trControl = control)

GLM = predict(fit.glm, TransBefore[-train, 2:18])
confusionMatrix(GLM, TransBefore$Arthritis[-train])


#Осуществляем ресэмплинг полученных моделей
caret.models <- list(CART = fit.cart, 
                     RF = fit.rf, GLM = fit.glm)
# ресэмплинг коллекции моделей
results <- resamples(caret.models)
# обобщение различий между моделями
summary(results)


#  Оценка доверительных интервалов и построение графика
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))
bwplot(results, scales=scales)
#можно вычислить статистическую значимость различий между моделями 
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)
#Составим таблицу прогноза всеми моделями для проверочных наблюдений
pred.fm <- data.frame(
  CART = predict(fit.cart, TransBefore[-train, 2:18]),
  RF = predict(fit.rf, TransBefore[-train, 2:18]),
  GLM = predict(fit.glm, TransBefore[-train, 2:18])
)

#ROC curve for train set
pred.rf.roc <- predict(fit.rf, TransBefore[, 2:18], type = "prob")
pred.cart.roc <- predict(fit.cart, TransBefore[, 2:18], type = "prob") 
pred.glm.roc <- predict(fit.glm, TransBefore[, 2:18], type = "prob") 

library(pROC) 
m2.roc <- roc(TransBefore$Arthritis, pred.rf.roc[, 1])
m1.roc <- roc(TransBefore$Arthritis, pred.glm.roc[, 1])
m3.roc <- roc(TransBefore$Arthritis, pred.cart.roc[, 1])

plot(m1.roc,  col = "blue",print.auc = TRUE, print.thres = TRUE)
plot(m2.roc ,  col = "red",print.auc = TRUE, print.thres = TRUE)
plot(m3.roc ,  col = "green",print.auc = TRUE)
legend("bottomright", c("CART","RF", "GLM"), lwd = 2,
       col = c(  "red", "blue"))

#ROC curve for test set
pred.rf.roc2 <- predict(fit.rf, TransBefore[-train, 2:18], type = "prob")
pred.cart.roc2 <- predict(fit.cart, TransBefore[-train, 2:18], type = "prob") 
pred.glm.roc2 <- predict(fit.glm, TransBefore[-train, 2:18], type = "prob") 

m.rf.roc <- roc(TransBefore$Arthritis[-train], pred.rf.roc2[, 1])
m.glm.roc<- roc(TransBefore$Arthritis[-train], pred.glm.roc2[, 1])
m.cart.roc <- roc(TransBefore$Arthritis[-train], pred.cart.roc2[, 1])

plot(m.rf.roc,  col = "blue",print.auc = TRUE, print.thres = TRUE)
plot(m.glm.roc,  col = "red",print.auc = TRUE, print.thres = TRUE)
plot(m.cart.roc,  col = "green",print.auc = TRUE) #не используем
