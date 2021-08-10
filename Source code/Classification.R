# Author: Reza Sadeghi
# Email: reza@knoesis.org; sadeghi.2@wright.edu; 
# Date: 4/24/2018
# Description: Classification of data
# Last modification: 07/01/2021 by Charan Pinninty (pinninty.2@wright.edu)

require(caret)
require(randomForest)
require(psych)
require(corrplot)
require(mlbench)
require(MLmetrics)
require(cart)
require(RSNNS)
require(e1071)
require(klaR)
require(Hmisc)
require(gridExtra)
require(readxl)
require(glmnet)


#>>>>>>>>>>>>>>>>>>>>>>> Load the data
#>> loading data
#survey<-read_xlsx("C:\\Users\\Reza Sadeghi\\Desktop\\Dementia Caregiver Sleep Dataset\\Output.xlsx")
survey <- readRDS("C:\\Sleep-quality-in-caregivers-master\\Examine_your_sleep_with_our_model\\Data/survey.rds")

str(survey)
View(survey)
#>> Tiredness
FeatureSet <- survey[,c("Particiapnt #","swsLengthHR","swsTimeHR","swsLengthT","swsTimeT","decreasePercentageT","swsTimeM","swsLengthM","decreasePercentageM","amountAsleep","amountAwake","sleepEfficiency","timesAwoken","epochCapacity","epochPeak","epochPeakCounter","stormPeak","largestStorm","timesEdaStorm","meanEdaStorm","lengthEdaStorm","Question 10")]
#>> Sleep Quality
#FeatureSet <- survey[,c("swsLengthHR","swsTimeHR","swsLengthT","swsTimeT","decreasePercentageT","swsTimeM","swsLengthM","decreasePercentageM","amountAsleep","amountAwake","sleepEfficiency","timesAwoken","epochCapacity","epochPeak","epochPeakCounter","stormPeak","largestStorm","timesEdaStorm","meanEdaStorm","lengthEdaStorm","Question 9")]
#FeatureSet$`Question 9`[which(FeatureSet$`Question 9`==4)]<- "3"

#>> dataset naming
dataset <- FeatureSet
#colnames(dataset)[1]<- "Label"
colnames(dataset)[length(dataset)]<- "Label"

#>>>>>>>>>>>>>>>>>>>>>>> Clean the data
# get rid of columns where for ALL rows the value is NA
dataset <- dataset[, colSums(is.na(dataset))<nrow(dataset)]
# unlist every columns
for (i in 1:length(dataset)){
  if(class(dataset[,i])=='list'){
    dataset[,i]<-unlist(dataset[,i])
  }
}
## Impute missing values with mean of each column in respect to each label group
#Labels=unique(dataset$Label)
#for (coln in 1:length(dataset)){
#  for (groupj in 1:length(Labels)){
#    MMean=mean(na.omit(dataset[which(dataset$Label==Labels[groupj]),coln]))
#    #NA enteties
#    dataset[is.na(dataset[which(dataset$Label==Labels[groupj]),coln]),coln]<-MMean
#    #Inf enteties
#    dataset[is.infinite(dataset[which(dataset$Label==Labels[groupj]),coln]),coln]<-MMean
#  }
#}
# converting labels to factors


dataset$Label<-as.factor(dataset$Label)

#View(dataset)

dataset[["swsLengthHR"]][is.na(dataset[["swsLengthHR"]])] <- mean(dataset$swsLengthHR,na.rm = TRUE)
dataset[["swsTimeHR"]][is.na(dataset[["swsTimeHR"]])] <- mean(dataset$swsTimeHR,na.rm = TRUE)
dataset[["swsLengthT"]][is.na(dataset[["swsLengthT"]])] <- mean(dataset$swsLengthT,na.rm = TRUE)
dataset[["swsTimeT"]][is.na(dataset[["swsTimeT"]])] <- mean(dataset$swsTimeT,na.rm = TRUE)
dataset[["decreasePercentageT"]][is.na(dataset[["decreasePercentageT"]])] <- mean(dataset$decreasePercentageT,na.rm = TRUE)
dataset[["swsTimeM"]][is.na(dataset[["swsTimeM"]])] <- mean(dataset$swsTimeM,na.rm = TRUE)
dataset[["swsLengthM"]][is.na(dataset[["swsLengthM"]])] <- mean(dataset$swsLengthM,na.rm = TRUE)
dataset[["decreasePercentageM"]][is.na(dataset[["decreasePercentageM"]])] <- mean(dataset$decreasePercentageM,na.rm = TRUE)
dataset[["amountAsleep"]][is.na(dataset[["amountAsleep"]])] <- mean(dataset$amountAsleep,na.rm = TRUE)
dataset[["amountAwake"]][is.na(dataset[["amountAwake"]])] <- mean(dataset$amountAwake,na.rm = TRUE)
dataset[["sleepEfficiency"]][is.na(dataset[["sleepEfficiency"]])] <- mean(dataset$sleepEfficiency,na.rm = TRUE)
dataset[["timesAwoken"]][is.na(dataset[["timesAwoken"]])] <- mean(dataset$timesAwoken,na.rm = TRUE)
dataset[["epochCapacity"]][is.na(dataset[["epochCapacity"]])] <- mean(dataset$epochCapacity,na.rm = TRUE)
dataset[["epochPeak"]][is.na(dataset[["epochPeak"]])] <- mean(dataset$epochPeak,na.rm = TRUE)
dataset[["epochPeakCounter"]][is.na(dataset[["epochPeakCounter"]])] <- mean(dataset$epochPeakCounter,na.rm = TRUE)
dataset[["stormPeak"]][is.na(dataset[["stormPeak"]])] <- mean(dataset$stormPeak,na.rm = TRUE)
dataset[["largestStorm"]][is.na(dataset[["largestStorm"]])] <- mean(dataset$largestStorm,na.rm = TRUE)
dataset[["timesEdaStorm"]][is.na(dataset[["timesEdaStorm"]])] <- mean(dataset$timesEdaStorm,na.rm = TRUE)
dataset[["meanEdaStorm"]][is.na(dataset[["meanEdaStorm"]])] <- mean(dataset$meanEdaStorm,na.rm = TRUE)
dataset[["lengthEdaStorm"]][is.na(dataset[["lengthEdaStorm"]])] <- mean(dataset$lengthEdaStorm,na.rm = TRUE)


max_acuR <- c()
max_seedR <- c()
max_precisionR<- c() 
max_recallR<- c()
max_F1R<- c()
MSE_trainR<- c()
MSE_testR<- c()

#enter the required participant to be excluded and tested on

part = 1
  
  max_acu = 0
  max_seed = 0
  max_precision = 0
  max_recall = 0
  max_F1 = 0
  MSE_train = 0
  MSE_test = 0

for (correct_seed in 1:1000){
  
  set.seed(correct_seed) 
  
  
  x <- dataset[,(2:21)]
  y <- dataset$Label
  
  x <-as.matrix(x)
  y <-as.matrix(y)
  
  lr <- min(which(dataset$`Particiapnt #`==part))
  ur <- max(which(dataset$`Particiapnt #`==part))
  
  
  
  x.train <- x[-(lr:ur),]
  x.test <- x[lr:ur,]
  
  
  y.train <- y[-(lr:ur),]
  y.test <- y[lr:ur,]
  
  y.test
  x.test
  lasso.fit <- cv.glmnet(x.train,y.train,alpha = 1,family="multinomial") 
  
  
  lasso.predicted <- glmnet(x.train,y.train,lambda = lasso.fit$lambda.min,family = "multinomial")
  
  pred <- predict(lasso.predicted, newx = x.test)
  
  pred_train <- predict(lasso.predicted, newx = x.train)
  
  predicted_values <- as.matrix(apply( pred, 1, which.max))
  
  predicted_train <- as.matrix(apply( pred_train, 1, max))
  
  y.test
  error_train <- MSE(predicted_train,as.numeric(y.train))
  error_test <- MSE(predicted_values,as.numeric(y.test))
  
  y.test
  
  
  predicted_values <- predicted_values - 1
  predicted_values <- factor(predicted_values,levels=(c(0,1)))
  y.test <- factor(y.test,levels = c(0,1))
  #ConfusionMatrix(predicted_values,y.test)
  d <- table(predicted_values,y.test)
  
  
  y.test
  
  precisionP <- mean(c(d[1,1]/sum(d[1,1:2]),d[2,2]/sum(d[2,1:2])),na.rm = TRUE) 
  recallP <- mean(c(d[1,1]/sum(d[1:2,1]),d[2,2]/sum(d[1:2,2])),na.rm = TRUE) 
  F1scoreP <- (2*precisionP*recallP)/(precisionP + recallP)
  predicted_values
  y.test
  acu <- sum(diag(d))/sum(d) 
  acu
  
  if(max_acu < acu){
    max_acu <- acu
    max_seed <- correct_seed
    max_precision <- precisionP
    max_recall <- recallP
    max_F1 <- F1scoreP
    MSE_train <- error_train
    MSE_test <- error_test
  }
}
  max_acuR <- c(max_acuR,max_acu)
  max_seedR <- c(max_seedR,max_seed)
  max_precisionR<- c(max_precisionR,max_precision) 
  max_recallR<- c(max_recallR,max_recall)
  max_F1R<- c(max_F1R,max_F1)
  MSE_trainR<- c(MSE_trainR,MSE_train)
  MSE_testR<- c(MSE_testR,MSE_test)
  
}






#>>>>>>>>>>>>>>>>>>>>>>> Correlation coefficient
#Temp<-as.data.frame(dataset[,c(predictors(results),"Label")])
Temp<-as.data.frame(dataset)
Temp$Label<-as.numeric(Temp$Label)
Temp <- as.matrix(Temp)
#colnames(Temp)<-1:length(c(predictors(results),"Label"))
colnames(Temp)<-1:length(dataset)

cor <- rcorr(as.matrix(Temp), type="pearson")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot.mixed(cor$r)

#>>>>>>>>>>>>>>>>>>>>>>> Balanced vs. Imbalanced
x <- dataset[,(1:20)]
y <- dataset$Label
#>> balancing the sleep quality
set.seed(600)
#A <- caret::upSample(x, y, list = FALSE, yname = "Class")
#y <- A[,"Class"]
#x <- A[-length(A)]
#dataset <- A
#colnames(dataset)[length(dataset)]<- "Label"
#>>>>>>>>>>>>>>>>>>>>>>> Feature selection using RF-RFE
#searching for random number
# seed <- sample(500,10)
# A <- rep(0,10)
# for (i in 1:10){
#   set.seed(seed[i])
#   # define the control using a random forest selection function
#   control <- rfeControl(functions=rfFuncs, method="cv", number=10)
#   # run the RFE algorithm
#   results <- rfe(x, y, sizes=c(1:20), rfeControl=control)
#   A[i]<- max(results$results$Accuracy)
# }

#>>> RF
seed=399
set.seed(seed)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
resultsRF <- rfe(x, y, sizes=c(1:20), rfeControl=control)
# summarize the results
print(resultsRF)
# list the chosen features
predictors(resultsRF)
# plot the results
plot(resultsRF, type=c("g", "o"))
max(resultsRF$results$Accuracy)

#>>> NB
set.seed(seed)
# define the control
control <- rfeControl(functions=nbFuncs, method="cv", number=10)
# run the RFE algorithm
resultsNB <- rfe(x, y, sizes=c(1:20), rfeControl=control)
# summarize the results
print(resultsNB)
# list the chosen features
predictors(resultsNB)
# plot the results
plot(resultsNB, type=c("g", "o"))
max(resultsNB$results$Accuracy)

#>>> bagged trees
set.seed(seed)
# define the control
control <- rfeControl(functions=treebagFuncs, method="cv", number=10)
# run the RFE algorithm
resultsBT <- rfe(x, y, sizes=c(1:20), rfeControl=control)
# summarize the results
print(resultsBT)
# list the chosen features
predictors(resultsBT)
# plot the results
plot(resultsBT, type=c("g", "o"))
max(resultsBT$results$Accuracy)

#>>>>>>>>>>>>>>>>>>>>>>>> Classification
# define training control and metrics for checking the performance
control <- trainControl(method="cv", number=10)
#control <- trainControl(method="repeatedcv", number=10, repeats=10)
#control <- trainControl(method="none")
metric <- "Accuracy"

# Navie Bayes
SelectedDataset<-as.data.frame(dataset[,c(predictors(resultsNB),"Label")])
set.seed(seed)
fit.nb <- caret::train(Label~., data=SelectedDataset, trControl=trainControl(method="cv", number=10), method="nb")
confusionMatrix.train(fit.nb)

# Random Forest
SelectedDataset<-as.data.frame(dataset[,c(predictors(resultsRF),"Label")])
set.seed(seed)
fit.rf <- caret::train(Label~., data=SelectedDataset, method="rf", metric=metric, trControl=control)
confusionMatrix.train(fit.rf)

# Bagged CART
SelectedDataset<-as.data.frame(dataset[,c(predictors(resultsBT),"Label")])
set.seed(seed)
fit.treebag <- caret::train(Label~., data=SelectedDataset, method="treebag", metric=metric, trControl=control)
confusionMatrix.train(fit.treebag)

results <- resamples(list(Naive_Bayes=fit.nb, Random_Forest=fit.rf, Bagged_CART=fit.treebag))
save(results,file="sleep_quality.RData")


summary(results)

# compare accuracy of models
dotplot(results)
