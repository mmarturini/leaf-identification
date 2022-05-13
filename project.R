library(tidyverse)
library(caret)
library(randomForest)
library(tictoc)

getwd()
setwd("C:/Users/matte/OneDrive/Desktop/Università/Trieste/First_year/ITML/Project/leaf")
leaf <- read.csv(file = "leaf.csv")
leaf <- leaf[-2] # deleting specimen number column
leaf$Species <- as.factor(leaf$Species) # making Species a factor variable so 
                                        # that it's going to be a class problem

table(leaf$Species)
table(ABOMBAZZA$Species)

View(leaf)
table(leaf$Species)
summary(leaf)




# OVERSAMPLING ---------------------------------------------------------
install.packages("caret")
library(caret)

data(oil)
View(fattyAcids)
View(oilType)

table(oilType)
new_oil <- downSample(fattyAcids, oilType)
table(new_oil$Class)



table(leaf$Species)
new_leaf <- upSample(leaf[-1], leaf$Species, list = TRUE)
overLeaf <- cbind(new_leaf[[2]],new_leaf[[1]])
names(overLeaf)[names(overLeaf) == "new_leaf[[2]]"] <- "Species"

colnames(overLeaf)
View(overLeaf)
str(overLeaf)

#differently from what specified on the paper, we have 30 and not 40 classes
#some are missing, and the specimens nmbers do not correspond to the ones
# specified on the readme file






# splitting intro train and test data ----------------------------------------
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(leaf$Species, p=.8, times = 1, list = FALSE)

leafTrain <- leaf[trainIndex,]
table(leafTrain$Species)
leafTest <- leaf[-trainIndex,]
table(leafTest$Species)







# ASSESSING RANDOM FOREST------------------------------------------------------

#### OOB error rate #####
library(randomForest)
?randomForest
m <- randomForest(Species~., data = leaf)
m
m$confusion
m$err.rate

confusion.matrix = m$confusion[,-ncol(m$confusion)] #remove the last column
1-sum(diag(confusion.matrix))/sum(confusion.matrix)


######## error rate from test data (TEST ERROR() #########
m <- randomForest(Species~., data = leafTrain)
test.confusion.matrix = table(leafTest$Species, predict(m, leafTest))
test.error = 1-sum(diag(test.confusion.matrix))/sum(test.confusion.matrix)
test.error #error rate
test.confusion.matrix


# let's try to do the same thing for different values of p in train/test indexing
errors <- rep(0,17)
i <- 1
for(r in seq(0.1,0.9,0.05)) {
  trainIndex <- createDataPartition(leaf$Species, p=r, times = 1, list = FALSE)
  m <- randomForest(Species~., data = leaf[trainIndex,])
  test.confusion.matrix <- table(leaf[-trainIndex,]$Species, predict(m, leaf[-trainIndex,]))
  test.error <- 1-sum(diag(test.confusion.matrix))/sum(test.confusion.matrix)
  errors[i] = test.error
  i <- i+1
}
plot(seq(0.1,0.9,0.05),errors)
lines(seq(0.1,0.9,0.05),errors)









# REPEATED CROSS VALIDATION FOR TUNING mtry PARAMETER
train_control <- trainControl(method = "repeatedcv", repeats = 4, number = 10) 

tunegrid <- expand.grid(.mtry=c(1:14))
{
tic("learning model:")
model <- train(Species ~., data = leaf,  
               method = "rf", 
               tuneGrid = tunegrid,
               trControl = train_control) 
toc()
}

# printing model performance metrics 
# along with other details 
print(model)





# TESTING SIMPLE CROSS VALIDATION
set.seed(3456)
train_control <- trainControl(method = "cv", number = 5)
tunegrid <- expand.grid(.mtry=c(1:14))
model <- train(Species ~., data = leafTrain,  
               method = "rf", 
               tuneGrid = tunegrid,
               trControl = train_control) 
print(model)
plot(model)
ggplot(model)





#---- TESTING PERFORMANCE INDEXES -------------------------------------------------

# other ways to compute the accuracy and confusion matrix
# test.error = 1-sum(diag(test.confusion.matrix))/sum(test.confusion.matrix)
# test.error
# #another way to compute confusion matrix
# cMat <- confusionMatrix(data = pred, reference = leafTest$Species)

cm = test.confusion.matrix
n = sum(cm)     # number of instances
nc = nrow(cm)   # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

accuracy = sum(diag)/n
accuracy

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)

# macro averaged metrics are just the average over all classes
# of the indexes above
macroPrecision = mean(precision, na.rm = TRUE)
macroRecall = mean(recall, na.rm = TRUE)
macroF1 = mean(f1, na.rm = TRUE)
data.frame(macroPrecision, macroRecall, macroF1)

# 1 vs all confusion matrices
oneVsAll = lapply(1 : nc,
                  function(i){
                    v = c(cm[i,i],
                          rowsums[i] - cm[i,i],
                          colsums[i] - cm[i,i],
                          n-rowsums[i] - colsums[i] + cm[i,i]);
                    return(matrix(v, nrow = 2, byrow = T))})
oneVsAll

#summing values from all the matrices (each entry)
s = matrix(0, nrow = 2, ncol = 2)
for(i in 1 : nc){s = s + oneVsAll[[i]]}
s

avgAccuracy = sum(diag(s)) / sum(s)
avgAccuracy

# micro averaged maetric
micro_prf = (diag(s) / apply(s,1, sum))[1];
micro_prf



# let's put everything into a function
# function that given the confusion matrix, computes accuracy, macroF1
# macroPrec, macroRec and microPerf 

perf_measures <- function(cm) {

  n = sum(cm)     # number of instances
  nc = nrow(cm)   # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  Accuracy = sum(diag)/n

  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 

  macroPrecision = mean(precision, na.rm = TRUE)
  macroRecall = mean(recall, na.rm = TRUE)
  macroF1 = mean(f1, na.rm = TRUE)

  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})

  s = matrix(0, nrow = 2, ncol = 2)
  for(i in 1 : nc){s = s + oneVsAll[[i]]}
  
  micro_prf = (diag(s) / apply(s,1, sum))[1];
  
  
  return(data.frame(Accuracy, macroPrecision, macroRecall, macroF1, micro_prf))
}


#let's try it out

{
trainIndex <- createDataPartition(leaf$Species, p=.8, times = 1, list = FALSE)
leafTrain <- leaf[trainIndex,]
leafTest <- leaf[-trainIndex,]
}

model <- randomForest(Species~., data = leafTrain)

test.confusion.matrix = table(leafTest$Species, predict(model, newdata = leafTest))

mina<-perf_measures(test.confusion.matrix)

final.df <- data.frame()
final.df <- rbind(final.df,mina)
final.df
?data.frame
#------------------------------------------------------------------------------



##### TESTING FOLD PARTITIONING  ----------------------------------------------
{
leaf <- read.csv(file = "leaf.csv")
leaf <- leaf[-2]
leaf$Species <- as.factor(leaf$Species)
}

ntrain <- length(leaf$Species) 
train.ext=createFolds(leaf$Species, k=5, returnTrain = TRUE)
test.ext=lapply(train.ext, function(x) (1:ntrain)[-x])
#str(train.ext)
# train.ext is a list of 5 elements, each element is an int array indicating the 
# positions in the leatning dataset leaf, in test.ext we have the correspondin
# test set for each of the on in train.ext

train.leaf <- leaf %>% slice(train.ext[[1]])
test.leaf <- leaf %>% slice(-train.ext[[1]])
leaf %>% slice(c(1,340))

view(train.int)

train.leaf[[1]] #access first element of list

#----------------------------------------------------------------------------




######## FINAL ASSESMENT ##############-----------------------------------
library(tidyverse)
library(caret)
library(randomForest)
library(tictoc)


{
leaf <- read.csv(file = "leaf.csv")
leaf <- leaf[-2] # deleting specimen number column
leaf$Species <- as.factor(leaf$Species) # making Species a factor variable so 
}

perf.df <- data.frame()

leaf = ABOMBAZZA

{
set.seed(941)
ntrain <- length(leaf$Species) 
train.ext=createFolds(leaf$Species, k=5, returnTrain = TRUE)
train_control <- trainControl(method = "cv", number = 5)
tunegrid <- expand.grid(.mtry=c(1:14))

perf.df.rf <- data.frame()

for (i in 1:5) {
  train.leaf <- leaf %>% slice(train.ext[[i]])
  test.leaf <- leaf %>% slice(-train.ext[[i]])

  model <- train(Species ~., data = train.leaf,  
                 method = "rf", 
                 tuneGrid = tunegrid,
                 trControl = train_control)
  
  test.confusion.matrix = table(test.leaf$Species, predict(model, newdata = test.leaf))
  performance <- perf_measures(test.confusion.matrix)
  perf.df.rf <- rbind(perf.df.rf, performance)
  
}
seed941.rf <- c(mean(perf.df.rf$Accuracy),sd(perf.df.rf$Accuracy))
}

colMeans(perf.df.rf)

sd(perf.df.rf$Accuracy)
sd(perf.df.rf$macroPrecision)
sd(perf.df.rf$macroRecall)
sd(perf.df.rf$macroF1)


campione.rf <- rbind(seed12.rf,seed162.rf, seed264.rf,seed345.rf,seed42.rf,seed57.rf,seed69.rf,seed69.rf,seed71.rf,seed888.rf,seed941.rf)
plot(campione.rf[,1])


#### PAIRET t-TEST, WILCOXON TEST #########----------------------------------------------------


shapiro.test(campione.tree[,1])
?shapiro.test
qqnorm(y=campione.tree[,1])
?qqplot

rm(campione.rf

t.test(campione.rf[,1], campione.boost[,1], paired = TRUE, alternative = "two.sided")
?t.test
#default conf level = 0,95 --> sign level alfa = 0.05


mean(campione.boost[,1])
mean(campione.rf[,1])

sd(campione.boost[,1])
sd(campione.rf[,1])

# for a given seed, let's say 69

?ggplot
?data.frame
finale.rf <- data.frame(mean(campione.rf[,1]), sd(campione.rf[,1]))
finale.tr <- data.frame(mean(campione.tree[,1]), sd(campione.tree[,1]))

names(finale.rf) <- c("Mean","Standard deviation")
names(finale.tr) <- c("Mean","Standard deviation")

finale <- rbind(finale.rf, finale.tr)

finale <- round(finale, digits = 2)

rownames(finale) <- c("Random forest","Tree")
colnames(finale) <- c("Mean", "SD")

finale





