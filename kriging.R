setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

library(openxlsx) # Open Excel files
library(glmnet) # Lasso Regression
library(dplyr) # data manipulation
library(gstat) # for kriging
library(Metrics) # for performance scores
library(tidyverse) # for manipulating the data frame
library(sf) # for converting coordinates
library(sp)
library(automap)
set.seed(42) # for reproducibility 
library(mice)

lassoKrigingPrediction = function(train,test, what_to_predict){

  
  results = array(1:2)
  
  matrixCovariates = as.matrix(as.data.frame(train)[,1:5])
  
  if(what_to_predict == "abundance"){
    matrixTarget = train$abundance  
  }
  else{
    matrixTarget = train$eveness
  }
  
  testCovariates = as.matrix(as.data.frame(test)[,1:5])
  
  cv = cv.glmnet(matrixCovariates, matrixTarget, alpha = 1, grouped = FALSE) 
  
  bestLambda = cv$lambda.min
  
  #plot(cv)
  
  bestModel = glmnet(matrixCovariates, matrixTarget, alpha = 1, lambda = bestLambda,standardize = FALSE)
  
  residuals = matrixTarget - predict(bestModel, s=bestLambda, newx =matrixCovariates ) # calculate the residuals from lasso 
  
  #print(mean(residuals))
  #hist(residuals)
  
  train$resid = residuals
  
  migliore <- autofitVariogram(resid ~ 1, train)

  residKriged = krige(resid ~ 1,locations = train,newdata = test,model = migliore$var_model,beta = 0)
  
  test$predi = predict(bestModel,s=bestLambda, newx = testCovariates) + residKriged$var1.pred
  
  #print(test$predi)
  
  if(what_to_predict == "abundance"){
    results[1] = rmse( test$predi,test$abundance)
    results[2] = smape( test$predi,test$abundance)  
  }
  else{
    matrixTarget = test$eveness
    results[1] = rmse( test$predi,test$eveness)
    results[2] = smape( test$predi,test$eveness)
  }
  
  return(results)
}

validation = function(train,nfolds, what_to_predict){
  
  # performs k fold cross validation
  # Args:
  #       train: training sf dataframe
  #       components: number of principal components to use for pcr. If you want to use lasso then put none
  #       nfolds: number of folds to use 
  #       method: lasso or pcr
  # Return: a matrix of size 2*2*3 where [,,j] corresponds to the mean and variance of the scores for variogram/covariance j 
  
  score1 = c(1:nfolds)
  score2 = c(1:nfolds)
  
  foldIndex = 1:nrow(train)
  
  for(j in 1:nfolds){
    
    trainFolds = train[foldIndex != j,]
    valFold = train[foldIndex == j,]
    
    scores = lassoKrigingPrediction(trainFolds,valFold, what_to_predict)
    
    score1[j] = scores[1]
    score2[j] = scores[2]
  }
  
  
  mean_rmse = mean(score1)
  mean_smape = mean(score2)
  
  return(list(mean_rmse,mean_smape))
}



###### 2024 ######

kriging_data = read.xlsx(xlsxFile = "kriging_data.xlsx", sheet = "Year 2024")

kriging_data.sf = st_as_sf(kriging_data,coords = c('lon','lat'), crs=4326)
kriging_data.sf = st_transform(kriging_data.sf , crs = 3995)

kriging_data.sf$Conductivity = log(kriging_data.sf$Conductivity)
kriging_data.sf$Temperature = log(kriging_data.sf$Temperature)
kriging_data.sf$Depth = log(kriging_data.sf$Depth)
kriging_data.sf$Altitude = scale(kriging_data.sf$Altitude)[1:nrow(kriging_data)]


krig_abund = validation(kriging_data.sf, nrow(kriging_data), "abundance")
krig_even = validation(kriging_data.sf, nrow(kriging_data), "eveness")


###### All Years ######

kriging_data = read.xlsx(xlsxFile = "kriging_data.xlsx", sheet = "All Years") #%>% na.omit()

miced_data = mice(kriging_data)
kriging_data = complete(miced_data)

kriging_data.sf = st_as_sf(kriging_data,coords = c('lon','lat'), crs=4326)
kriging_data.sf = st_transform(kriging_data.sf , crs = 3995)

kriging_data.sf$Conductivity = log(kriging_data.sf$Conductivity)
kriging_data.sf$Temperature = log(kriging_data.sf$Temperature)
kriging_data.sf$Depth = log(kriging_data.sf$Depth)
kriging_data.sf$Altitude = scale(kriging_data.sf$Altitude)[1:nrow(kriging_data)]


krig_abund = validation(kriging_data.sf, nrow(kriging_data), "abundance")
krig_even = validation(kriging_data.sf, nrow(kriging_data), "eveness")





