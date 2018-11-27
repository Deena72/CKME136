
# Loading the dataset


load("~/Desktop/newsreg.Rda")



# Sampling the dataset into 70% training data and 30% test data


set.seed(350)

newsregTrain <- sample(nrow(newsreg),as.integer(nrow(newsreg)*0.70))
train.newsreg = newsreg[newsregTrain,]
test.newsreg = newsreg[-newsregTrain,]


#### LINEAR REGRESSION ####
# Fit a linear model with all the variables 
#Dependent variable is log(shares) and all other explanatory variables from the clean newserg dataset are the predictors.

#Train the model

set.seed(350)
Model_Linear <- lm(shares ~ ., data = train.newsreg)   ## training the model
summary(Model_Linear)


#Test the model and calculate RMSE

pred_reg <-  predict(Model_Linear, test.newsreg)  ## apply to test data
actual <- exp(test.newsreg$shares)  ### to return data into its originial values
predicted <- exp(pred_reg)   ## to get the inverse log of the predicted values
cat("RMSE = ", sqrt(mean((actual - predicted)^2)))    

##### We can also calculate RMSE on the logs to be able to compare performance of the model to other literature that calculated RMSE on the log directly 
cat("RMSE of log = ", sqrt(mean((test.newsreg$shares - pred_reg)^2)))  ### calculate RMSE  on the logs 


#Visual Comparison of the models through boxplots of the logs

summary(log(actual))
summary(log(predicted))    ## means and medians of actual and predicted models are pretty much close
par(mfrow=c(1,2))
boxplot(log(actual), main="Actual LR", ylim= c(3,14) )
boxplot(log(predicted), main= "Predicted LR", ylim= c(3,14))



#### RANDOM FOREST REGRESSION ####

library(randomForest)

# Fit a Random Forest model with all the variables
#Dependent variable is shares and all other explanatory variables from the clean newserg dataset are the predictors.

#Train the model

set.seed(350)
RFreg <- randomForest(shares ~ ., data = train.newsreg, ntree=30)   ## training the model
importance(RFreg)    ## to Know which variables are important


#Test the model and calculate RMSE

pred_regRF <- predict(RFreg, test.newsreg)  ## apply to test data
actualRF <- exp(test.newsreg$shares)  ### to return data into its originial values
predictedRF <- exp(pred_regRF)   ## to get the inverse log of the predicted values
cat ("RMSE Random Forest = ", sqrt(mean((actualRF - predictedRF)^2)))  

###RMSE calculated on the logs
cat ("RMSE log Random Forest Model = ",sqrt(mean((test.newsreg$shares - pred_regRF)^2)))  


#Visual Comparison of the models through boxplots of the logs

summary(log(actualRF))
summary(log(predictedRF))
par(mfrow=c(1,2))
boxplot(log(actualRF), main= "Actual RF", ylim= c(3,14))
boxplot(log(predictedRF), main= "Predicted RF", ylim= c(3,14))


##### CROSS VALIDATION ####

library(caret)


#### Cross Validation for Linear Model ####

set.seed(350)
Model_LinearCV <- train(shares ~ . , data = train.newsreg, method="lm", 
                      trControl= trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE))
summary(Model_LinearCV)
print(Model_LinearCV)  
results <- Model_LinearCV$results   ### RESULTS OF CROSS Validation  (Mean, SD, etc..)
results
print (Model_LinearCV$resample)    
mean(Model_LinearCV$resample[,1])    # getting mean of the RMSE of the 10 folds manually 
pred_regLinearCV = predict(Model_LinearCV, test.newsreg)  ## apply to test data
actualLinearCV <- exp(test.newsreg$shares)  ### to return data into its originial values
predictedLinearCV <- exp(pred_regLinearCV)   ## to get the inverse log of the predicted values
cat(sep="\n")
cat ("RMSE Cross-Validated Linear Model = ",sqrt(mean((actualLinearCV - predictedLinearCV)^2)))   
cat(sep="\n")
cat ("RMSE log Cross-Validated Linear Model = ", sqrt(mean((test.newsreg$shares - pred_regLinearCV)^2)))


#### Cross Validation for Random Forest Model ####

Model_RFCV <- train(shares ~ . , data = train.newsreg, method="rf", ntree= 30,
                      trControl= trainControl(method = "cv", number = 3, savePredictions = TRUE, classProbs = TRUE))
pred_regRFCV = predict(Model_RFCV, test.newsreg)  ## apply to test data
actualRFCV <- exp(test.newsreg$shares)  ### to return data into its originial values
predictedRFCV <- exp(pred_regRFCV)   ## to get the inverse log of the predicted values
cat(sep="\n")
cat ("RMSE Cross-Validated RF Model = ",sqrt(mean((actualRFCV - predictedRFCV)^2)))   
cat(sep="\n")
cat ("RMSE log Cross-Validated RF Model = ",sqrt(mean((test.newsreg$shares - pred_regRFCV)^2)))  
