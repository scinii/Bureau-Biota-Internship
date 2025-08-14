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
set.seed(42) # for reproducibility 
library(mice)
library(tidysdm)
library("car")

lassoKrigingPrediction = function(train, test, what_to_predict){

  
  results = array(1:2)
  
  matrixCovariates = as.matrix(as.data.frame(train)[,1:5])
  
  matrixTarget = train[[what_to_predict]]
  
  testCovariates = as.matrix(as.data.frame(test)[,1:5])
  
  testCovariates = scale(testCovariates, center = apply(matrixCovariates,2,mean), scale = apply(matrixCovariates,2,sd))
  
  matrixCovariates = scale(matrixCovariates)
  
  cv = cv.glmnet(matrixCovariates, matrixTarget, alpha = 1, grouped = FALSE) 
  
  bestLambda = cv$lambda.min
  
  bestModel = glmnet(matrixCovariates, matrixTarget, alpha = 1, lambda = bestLambda,standardize = FALSE)
  
  residuals = matrixTarget - predict(bestModel, s=bestLambda, newx =matrixCovariates ) # calculate the residuals from lasso 
  
  print(mean(residuals))
  hist(residuals)
  print(shapiro.test(residuals))
  qqPlot(residuals)
  
  train$resid = residuals
  
  migliore <- autofitVariogram(resid ~ 1, train)
  
  residKriged = krige(resid ~ 1,locations = train,newdata = test,model = migliore$var_model,beta = 0)
  
  test$predi = (predict(bestModel,s=bestLambda, newx = testCovariates) + residKriged$var1.pred) 
  
  test_true = test[[what_to_predict]] 
  
  results[1] = Metrics::rmse( test$predi,test_true)
  results[2] = Metrics::smape( test$predi,test_true)  
  
  
  return(list(results,bestModel))
}

temporal_distancing = function(sf){
  
  distancing = seq(6,1)*1e6
  
  years = seq(2018,2023)
  
  for(i in 1:6){
    
    year = years[i]
    
    print(sf[sf$Year == year,]$geometry)
    
    sf[sf$Year == year,]$geometry = sf[sf$Year == year,]$geometry + distancing[i]
    
    
    print(sf[sf$Year == year,]$geometry)
  }
  
  return(sf)
}

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

krig_N1 = lassoKrigingPrediction(train_sf, test_sf, "N1")

coef(krig_N1[[2]])

krig_N2 = lassoKrigingPrediction(train_sf, test_sf, "N2")

coef(krig_N2[[2]])




