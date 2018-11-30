


# Loading the dataset and calling required libraries


load("~/Desktop/newsclass.Rda")
library(stats)
library(ROCR)
library(pROC)
library(class)
library(gmodels)
library(randomForest)
library(lattice)



# Checking the distribution of shares and its balance

shares <- factor(newsclass$shares, levels = c("0", "1"), labels = c("NonPopular", "Popular"))
round(prop.table(table(shares)) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place
barchart(shares)


# Creating training and test datasets

set.seed(350)

newsclassTrain <- sample(nrow(newsclass),as.integer(nrow(newsclass)*0.70))
train.newsclass = newsclass[newsclassTrain,]
test.newsclass = newsclass[-newsclassTrain,]

##create train and test labels 
train_labels <- as.character(train.newsclass[,18])   
test_labels <- as.character(test.newsclass[, 18])


#### KNN ####

#Transforming factor data into numeric to be able to run KNN

newsclassKNN <- newsclass
newsclassKNN$is_weekend <- as.numeric(newsclassKNN$is_weekend)
newsclassKNN$video <- as.numeric(newsclassKNN$video)
colnames(newsclassKNN)
newsclassKNN$data_channel_is_lifestyle <- as.numeric(newsclassKNN$data_channel_is_lifestyle)
newsclassKNN$data_channel_is_entertainment <- as.numeric(newsclassKNN$data_channel_is_entertainment)
newsclassKNN$data_channel_is_bus <- as.numeric(newsclassKNN$data_channel_is_bus)
newsclassKNN$data_channel_is_socmed <- as.numeric(newsclassKNN$data_channel_is_socmed)
newsclassKNN$data_channel_is_world <- as.numeric(newsclassKNN$data_channel_is_world)

str(newsclassKNN)   ## all numeric except shares


#Sampling the KNN dataset into 70% training data and 30% test data

set.seed(350)
newsclassTrainKNN <- sample(nrow(newsclassKNN),as.integer(nrow(newsclassKNN)*0.70))
train.newsclassKNN = newsclassKNN[newsclassTrainKNN,]
test.newsclassKNN = newsclassKNN[-newsclassTrainKNN,]


#Create train and test labels for KNN

train_labelsKNN <- as.character(train.newsclass[,18])   
test_labelsKNN <- as.character(test.newsclass[, 18])


#Remove the labels of train and test datasets to run KNN

set.seed(350)
train.newsclassKNN <- train.newsclassKNN[,-18]  
test.newsclassKNN <- test.newsclassKNN[,-18] 
test_predKNN <- knn(train = train.newsclassKNN, test = test.newsclassKNN,cl = train_labelsKNN, k=10) 


#Check KNN accuracy

KNN.Confusion <- CrossTable(x=test_labelsKNN, y=test_predKNN, prop.chisq=FALSE)
AccuracyKNN <- (KNN.Confusion$t[1]+KNN.Confusion$t[4])/(KNN.Confusion$t[1]+KNN.Confusion$t[2]+KNN.Confusion$t[3]+KNN.Confusion$t[4]) #(TP+TN/Total) 
cat("KNN Model Accuarcy = ",round(AccuracyKNN,4))   
precisionKNN <- KNN.Confusion$t[4]/(KNN.Confusion$t[2]+KNN.Confusion$t[4])
cat("KNN Model Precision = ", round(precisionKNN,4))  
recallKNN <- KNN.Confusion$t[4]/(KNN.Confusion$t[3]+KNN.Confusion$t[4])
cat("KNN Model Recall = ", round(recallKNN,4))

#### LOGISTIC REGRESSION ####

#Train the model

set.seed(350)
LR_Model <- glm(shares ~ . , data=train.newsclass, family=binomial(link="logit")) ### run logistic regression on training dataset
summary(LR_Model)


#Test the model and calculate accuracy

test_predLR <- round(predict(LR_Model, test.newsclass, type="response"))
Confusion_LR <- CrossTable(x=test_labels, y=test_predLR, prop.chisq=FALSE)
Accuracy_LR <- (Confusion_LR$t[1]+Confusion_LR$t[4])/(Confusion_LR$t[1]+Confusion_LR$t[2]+Confusion_LR$t[3]+Confusion_LR$t[4]) #(TP+TN/Total)    
cat("LR Model Accuarcy = ", round(Accuracy_LR,4))   
precisionLR <- Confusion_LR$t[4]/(Confusion_LR$t[2]+Confusion_LR$t[4])
cat("LR Model Precision = ", round(precisionLR,4))  
recallLR <- Confusion_LR$t[4]/(Confusion_LR$t[3]+Confusion_LR$t[4])
cat("LR Model Recall = ", round(recallLR,4))

#### RANDOM FOREST CLASSIFIER ####

#Train the model

set.seed(350)
RF_Model <- randomForest(shares ~ . , train.newsclass, ntree=500)
importance(RF_Model)


#Check best number of trees

plot(RF_Model)


#Test the model and calculate accuracy

test_predRF <- predict(RF_Model, test.newsclass, type="class")
Confusion_RF <- CrossTable(x=test_labels, y=test_predRF, prop.chisq=FALSE)
Accuracy_RF <- (Confusion_RF$t[1]+Confusion_RF$t[4])/(Confusion_RF$t[1]+Confusion_RF$t[2]+Confusion_RF$t[3]+Confusion_RF$t[4]) #(TP+TN/Total)    #0.630
cat("RF Model Accuarcy = ",round(Accuracy_RF,4))   
precisionRF <- Confusion_RF$t[4]/(Confusion_RF$t[2]+Confusion_RF$t[4])
cat("RF Model Precision = ", round(precisionRF,4))  
recallRF <- Confusion_RF$t[4]/(Confusion_RF$t[3]+Confusion_RF$t[4])
cat("RF Model Recall = ", round(recallRF,4))

#### DRAWING ROC CURVES AND COMPARING MODELS AUC ####

par(mfrow=c(2,2))


### ROC CURVE for KNN

color.knn<-'#ef696a'       #color of the curve for KNN
test_labelsKNN <- as.numeric(test_labels)   ## to be able to draw ROC
test_predKNN <- as.numeric(test_predKNN)
for (i in 1:11893) {
  ifelse (test_predKNN[i] == 1, test_predKNN[i]<- 0,test_predKNN[i] <-1)
}                  #### turn 1, 2 levels into 0, 1 levels

newscla.knn.roc <- roc(test_labelsKNN,test_predKNN)
plot(newscla.knn.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.knn, print.thres=TRUE)
text(0.55,0.8,"KNN")


### ROC CURVE for Logistic Regression

color.LR <-'#adef69'       #color of the curve for LR
test_labelsLR <- as.numeric(test_labels)

newscla.LR.roc <- roc(test_labelsLR,test_predLR)
plot(newscla.LR.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.LR, print.thres=TRUE)
text(0.55,0.8,"LR")


### ROC CURVE for Random Forest

color.RF <-'#69adef'       #color of the curve for RF
test_labelsRF <- as.numeric(test_labels)
test_predRF <- as.numeric(test_predRF)

newscla.RF.roc <- roc(test_labelsRF,test_predRF)
plot(newscla.RF.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.RF, print.thres=TRUE)
text(0.55,0.8,"RF")


### Comparing ROC curves of the models

plot(NA, xlab="False Positive Rate", ylab="True Positive Rate", xlim = c(0, 1), ylim = c(0, 1))
plot(performance(prediction(test_predKNN,test_labelsKNN),'tpr','fpr'),
     col=color.knn, lwd=3, add=TRUE
)
text(0.55,0.6,"KNN",col=color.knn)

plot(performance(prediction(test_predLR,test_labelsLR),'tpr','fpr'),
     col=color.LR, lwd=7, add=TRUE
)
text(0.4,0.8,"LR",col=color.LR)

plot(performance(prediction(test_predRF,test_labelsRF),'tpr','fpr'),
     col=color.RF, lwd=3, add=TRUE
)
text(0.3,0.7,"RF",col=color.RF)



##### CROSS VALIDATION ####

library(caret)
library(e1071)


#### KNN CROSS VALIDATION ####

ctrlKNN <- trainControl(method = "cv", number = 10)
Model_KNNCV <- train(train.newsclassKNN,train_labelsKNN, method='knn', trControl=ctrlKNN)
print(Model_KNNCV)
print(Model_KNNCV$resample)
mean(Model_KNNCV$resample[,1])  ###manually calculating mean of accuracies of the different folds  0.5053 
test_predKNNCV <- predict(Model_KNNCV, test.newsclassKNN) 
KNNCV.Confusion <- CrossTable(x=test_labelsKNN, y=test_predKNNCV, prop.chisq=FALSE)
AccuracyKNNCV <- (KNNCV.Confusion$t[1]+KNNCV.Confusion$t[4])/(KNNCV.Confusion$t[1]+KNNCV.Confusion$t[2]+KNNCV.Confusion$t[3]+KNNCV.Confusion$t[4]) #(TP+TN/Total) ###
cat("KNN Cross Validated Model Accuarcy = ",round(AccuracyKNN,4))   
precisionKNNCV <- KNNCV.Confusion$t[4]/(KNNCV.Confusion$t[2]+KNNCV.Confusion$t[4])
cat("KNN Cross Validated Model Precision = ", round(precisionKNNCV,4))  
recallKNNCV <- KNNCV.Confusion$t[4]/(KNNCV.Confusion$t[3]+KNNCV.Confusion$t[4])
cat("KNN Cross Validated Model Recall = ", round(recallKNNCV,4))


#### CROSS VALIDATION FOR LOGISTIC REGRESSION ####

set.seed(350)
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
Model_LRCV <- train(shares ~ . , data = train.newsclass, method="glm", family="binomial",
                    trControl = ctrl, tuneLength = 5)
print(Model_LRCV)   
print(Model_LRCV$resample)
mean(Model_LRCV$resample[,1])  ###manually calculating mean of accuracies of the different folds                
summary(Model_LRCV)
test_predLRCV <- predict(Model_LRCV, test.newsclass) 
Confusion_LRCV <- CrossTable(x=test_labels, y=test_predLRCV, prop.chisq=FALSE)
Accuracy_LRCV <- (Confusion_LRCV$t[1]+Confusion_LRCV$t[4])/(Confusion_LRCV$t[1]+Confusion_LRCV$t[2]+Confusion_LRCV$t[3]+Confusion_LRCV$t[4]) #(TP+TN/Total)    
cat("Logistic Regression Cross Validated Model Accuarcy = ", round(Accuracy_LRCV,4))   
precisionLRCV <- Confusion_LRCV$t[4]/(Confusion_LRCV$t[2]+Confusion_LRCV$t[4])
cat("Logistic Regression Cross Validated Model Precision = ", round(precisionLRCV,4))  
recallLRCV <- Confusion_LRCV$t[4]/(Confusion_LRCV$t[3]+Confusion_LRCV$t[4])
cat("Logistic Regression Cross Validated Model Recall = ", round(recallLRCV,4))


#### CROSS VALIDATION FOR RANDOM FOREST CLASSIFIER ####

set.seed(350)
ctrlRF <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
Model_RFCV <- train(shares ~ . , data = train.newsclass, method="rf", ntree= 500,
                    trControl = ctrlRF)
print(Model_RFCV)    
print(Model_RFCV$resample)
mean(Model_RFCV$resample[,1])
test_predRFCV <- predict(Model_RFCV, test.newsclass) 
Confusion_RFCV <- CrossTable(x=test_labels, y=test_predRFCV, prop.chisq=FALSE)
Accuracy_RFCV <- (Confusion_RFCV$t[1]+Confusion_RFCV$t[4])/(Confusion_RFCV$t[1]+Confusion_RFCV$t[2]+Confusion_RFCV$t[3]+Confusion_RFCV$t[4]) #(TP+TN/Total)    
cat("Random Forest Cross Validated Model Accuarcy = ", round(Accuracy_RFCV,4))   
precisionRFCV <- Confusion_RFCV$t[4]/(Confusion_RFCV$t[2]+Confusion_RFCV$t[4])
cat("Random Forest Cross Validated Model Precision = ", round(precisionRFCV,4))  
recallRFCV <- Confusion_RFCV$t[4]/(Confusion_RFCV$t[3]+Confusion_RFCV$t[4])
cat("Random Forest Cross Validated Model Recall = ", round(recallRFCV,4))
