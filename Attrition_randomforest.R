install.packages("randomForest")
install.packages("ROCR")
library(randomForest)
data1 <- read.csv(file.choose(), header = TRUE)
head(data1)
str(data1)
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
model1 <- randomForest(Attrition~ ., data = TrainSet, importance = TRUE)
model1
model2 <- randomForest(Attrition ~ ., data = TrainSet, ntree = 500, mtry = 7 , importance = TRUE)
model2
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Attrition)  
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$Attrition)                    
table(predValid,ValidSet$Attrition)
importance(model2)        
varImpPlot(model2)    
a=c()
i=5
for (i in 3:11) {
  model3 <- randomForest(Attrition ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Attrition)
}
a
plot(3:11,a)
## plotting ROC Curve

library(ROCR)

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(model2,ValidSet[,-31],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(ValidSet$Attrition)
# For each class
for (i in 1:2)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(ValidSet[,31]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
