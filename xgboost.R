library(tidyverse)
library(caret)
library(xgboost)
library(tictoc)

set.seed(941)
trainIndex <- createDataPartition(leaf$Species, p=.8, times = 1, list = FALSE)
leafTrain <- leaf[trainIndex,]
leafTest <- leaf[-trainIndex,]

{
tic("Boosting")
model <- train(
  Species ~., data = leafTrain, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
)
toc()
}

print(model)

test.confusion.matrix = table(leafTest$Species, predict(model, leafTest))
performance <- perf_measures(test.confusion.matrix)
performance

seed.123.boost <- performance
seed.162.boost <- performance
seed.264.boost <- performance
seed.345.boost <- performance
seed.42.boost <- performance
seed.57.boost <- performance
seed.69.boost <- performance
seed.71.boost <- performance
seed.888.boost <- performance
seed.941.boost <- performance



campione.boost <- rbind(seed.123.boost$Accuracy,seed.162.boost$Accuracy, seed.264.boost$Accuracy,seed.345.boost$Accuracy,seed.42.boost$Accuracy,seed.57.boost$Accuracy,seed.69.boost$Accuracy,seed.71.boost$Accuracy,seed.888.boost$Accuracy,seed.941.boost$Accuracy)
campione.boost
plot(campione.boost[,1])
campione.boost[9,1] <- 0.854
campione.boost <- rbind(campione.boost, 0.874589)
campio