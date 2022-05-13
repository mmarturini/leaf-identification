require(rpart)


# testing caret for rpart ------------------------------------------
train_control <- trainControl(method = "cv", number = 5)
tunegrid <- expand.grid(.cp=seq(from = 0.005, to=0.04, by = 0.001))
model <- train(Species ~., data = ABOMBAZZA,  
               method = "rpart", 
               tuneGrid = tunegrid,
               trControl = train_control)
print(model)
ggplot(model)
# ------------------------------------------------------------------


# DOING NESTED CV FOR THE SINGLE TREE

leaf = ABOMBAZZA

{
set.seed(162)
  
ntrain <- length(leaf$Species) 
train.ext=createFolds(leaf$Species, k=5, returnTrain = TRUE)
train_control <- trainControl(method = "cv", number = 5)
tunegrid <- expand.grid(.cp=seq(from = 0.005, to=0.04, by = 0.001))

perf.df.tr <- data.frame()

for (i in 1:5) {
  train.leaf <- leaf %>% slice(train.ext[[i]])
  test.leaf <- leaf %>% slice(-train.ext[[i]])
  
  model <- train(Species ~., data = train.leaf,  
                 method = "rpart", 
                 tuneGrid = tunegrid,
                 trControl = train_control)
  
  test.confusion.matrix = table(test.leaf$Species, predict(model, newdata = test.leaf))
  performance <- perf_measures(test.confusion.matrix)
  perf.df.tr <- rbind(perf.df.tr, performance)
}
seed162.tr <- c(mean(perf.df.tr$Accuracy),sd(perf.df.tr$Accuracy))
}

ggplot(model)

campione.tree <- rbind(seed12.tr,seed162.tr, seed264.tr,seed345.tr,seed42.tr,seed57.tr,seed69.tr,seed69.tr,seed71.tr,seed888.tr,seed941.tr)
plot(campione.tree[,1])
  


colMeans(perf.df.tr)

sd(perf.df.tr$Accuracy)
sd(perf.df.tr$macroPrecision)
sd(perf.df.tr$macroRecall)
sd(perf.df.tr$macroF1)







campione.rf
campione.tree




{
  trainIndex <- createDataPartition(leaf$Species, p=.8, times = 1, list = FALSE)
  leafTrain <- leaf[trainIndex,]
  leafTest <- leaf[-trainIndex,]
}
tr <- tree(Species~.,leafTrain)
predicted <- predict(tr, newdata = leafTest, type = "class")
test.confusion.matrix = table(leafTest$Species, predicted)
test.error = 1-sum(diag(test.confusion.matrix))/sum(test.confusion.matrix)
test.error




