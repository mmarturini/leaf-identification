{
  leaf <- read.csv(file = "leaf.csv")
  leaf <- leaf[-2] # deleting specimen number column
  leaf$Species <- as.factor(leaf$Species) # making Species a factor variable so 
}

leaf = ABOMBAZZA

m = randomForest(Species~., leaf)
m$importance
varImpPlot(m)
?varImpPlot


set.seed(4543)

m <- randomForest(Species ~ ., data=leaf, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)

imp <- varImpPlot(m) # let's save the varImp object

# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
imp$varnames
rownames(imp) <- NULL  
imp$var_categ <- c(rep("Shape",7),rep("Texture",7)) # 1 = shape, 2 = texture


# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 

ggplot(imp, aes(x=reorder(varnames, imp$MeanDecreaseGini), weight=imp$MeanDecreaseGini, fill=as.factor(var_categ))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name")


ggplot(imp, aes(x=reorder(varnames, MeanDecreaseGini), y=MeanDecreaseGini, color=as.factor(var_categ))) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=MeanDecreaseGini)) +
  scale_color_discrete(name="Variable Group") +
  ylab("Mean decrease Gini") +
  xlab("Variable Name") +
  coord_flip()






data.frame(table(leaf$Species))


install.packages("writexl") 
library(writexl)
write_xlsx(data.frame(table(leaf$Species)),"tabella.xlsx")

