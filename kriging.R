setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory


install.packages("pak")
pak::pak("tidymodels/tidymodels")

library(openxlsx) # Open Excel files
library(glmnet) # Lasso Regression
library(dplyr) # data manipulation
library(gstat) # for kriging
library(Metrics) # for performance scores
library(tidyverse) # for manipulating the data frame
library(sf) # for converting coordinates
library(sp)
library(automap)
library(mice)
library(tidysdm)
library("car")

set.seed(0) # for reproducibility 

temporal_distancing = function(sf){
  
  distancing = seq(6,1)*1e5
  
  years = seq(2018,2023)
  
  for(i in 1:6){
    
    year = years[i]
    
    print(sf[sf$Year == year,]$geometry)
    
    sf[sf$Year == year,]$geometry = sf[sf$Year == year,]$geometry + distancing[i]
    
    
    print(sf[sf$Year == year,]$geometry)
  }
  
  return(sf)
}

lassoKrigingPrediction = function(train, test, what_to_predict, plot_bool){

  
  results_lasso = array(1:2)
  results_kriging = array(1:2)
  
  matrixCovariates = as.matrix(as.data.frame(train)[,1:5])
  
  matrixTarget = train[[what_to_predict]]
  
  testCovariates = as.matrix(as.data.frame(test)[,1:5])
  
  testCovariates = scale(testCovariates, center = apply(matrixCovariates,2,mean), scale = apply(matrixCovariates,2,sd))
  
  matrixCovariates = scale(matrixCovariates)
  
  cv = cv.glmnet(matrixCovariates, matrixTarget, alpha = 1, grouped = FALSE) 
  
  bestLambda = cv$lambda.min
  
  bestModel = glmnet(matrixCovariates, matrixTarget, alpha = 1, lambda = bestLambda,standardize = FALSE)
  
  residuals = matrixTarget - predict(bestModel, s=bestLambda, newx =matrixCovariates ) # calculate the residuals from lasso 
  
  if(plot_bool == TRUE){
    
    print(mean(residuals))
    hist(residuals)
    print(shapiro.test(residuals))
    qqPlot(residuals)
    
  }
  
  train$resid = residuals
  
  migliore <- autofitVariogram(resid ~ 1, train)
  
  residKriged = krige(resid ~ 1,locations = train,newdata = test,model = migliore$var_model,beta = 0)
  
  lasso_prediciton = predict(bestModel,s=bestLambda, newx = testCovariates)
  
  test$predi = lasso_prediciton + residKriged$var1.pred
  
  test_true = test[[what_to_predict]] 
  
  results_lasso[1] = Metrics::rmse( lasso_prediciton,test_true)
  results_lasso[2] = Metrics::smape( lasso_prediciton,test_true)  
  
  results_kriging[1] = Metrics::rmse( test$predi,test_true)
  results_kriging[2] = Metrics::smape( test$predi,test_true)  
  
  
  return(list(results_kriging, results_lasso,bestModel))
}


validation = function(train,nfolds, what_to_predict){
  
  
  rmse_kriging = c(1:nfolds)
  smape_kriging = c(1:nfolds)
  rmse_lasso = c(1:nfolds)
  smape_lasso = c(1:nfolds)
  
  foldIndex = 1:nrow(train)
  
  for(j in 1:nfolds){
    
    trainFolds = train[foldIndex != j,]
    valFold = train[foldIndex == j,]
    
    scores = lassoKrigingPrediction(trainFolds,valFold, what_to_predict, FALSE)
    
    
    scores_kriging = scores[[1]]
    scores_lasso = scores[[2]]
    
    rmse_kriging[j] = scores_kriging[1]
    smape_kriging[j] = scores_kriging[2]
    
    rmse_lasso[j] = scores_lasso[1]
    smape_lasso[j] = scores_lasso[2]
    
    
  }
  
  kriging_scores = c(mean(rmse_kriging), mean(smape_kriging))
  lasso_scores = c(mean(rmse_lasso), mean(smape_lasso))
  
  return(list(kriging_scores, lasso_scores))
  
}


###### Year 2024 ######

#kriging_data_24 = read.xlsx(xlsxFile = "kriging_data.xlsx", sheet = "Year 2024") 

#kriging_data_24.sf = st_as_sf(kriging_data_24,coords = c('lon','lat'), crs=4326)
#kriging_data_24.sf = st_transform(kriging_data_24.sf , crs = 3995)

#krig_24_N1 = validation(kriging_data_24.sf, nrow(kriging_data_24.sf), "N1")
#krig_24_N2 = validation(kriging_data_24.sf, nrow(kriging_data_24.sf), "N2")

###### All Years ######

kriging_data = read.xlsx(xlsxFile = "kriging_data.xlsx", sheet = "All Years") 

miced_data = mice(kriging_data)
kriging_data = complete(miced_data)

kriging_data.sf = st_as_sf(kriging_data,coords = c('lon','lat'), crs=4326)
kriging_data.sf = st_transform(kriging_data.sf , crs = 3995)


hello = temporal_distancing(kriging_data.sf)

block_initial <- spatial_initial_split(hello,prop = 0.2, spatial_block_cv)

train_sf = training(block_initial)

test_sf = testing(block_initial)

krig_N1 = lassoKrigingPrediction(train_sf, test_sf, "N1", TRUE)

coef(krig_N1[[3]])

krig_N2 = lassoKrigingPrediction(train_sf, test_sf, "N2", TRUE)

coef(krig_N2[[3]])





