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
library(rstanarm)
library(bayestestR)
library(bayesplot)
library(insight)
library(broom)

set.seed(0) # for reproducibility 


# FUNCTIONS FOR KRIGING #

temporal_distancing <-  function(sf){
  
  " 
  This function separates applies the separation method of the lakes form different
  years described in the report.
    
  "
  
  distancing <-  seq(6,1)*1e5
  
  years <-  seq(2018,2023)
  
  for(i in 1:6){
    
    year <-  years[i]
    
    sf[sf$Year == year,]$geometry <-  sf[sf$Year == year,]$geometry + distancing[i]
    
  }
  
  return(sf)
}

lassoKrigingPrediction = function(train, test, what_to_predict, plot_bool){
  
  # performs Kriging with external drift provided by lasso regression
  # Args:
  #       train: training sf dataframe
  #       test: testing sf dataframe
  #       what_to_predict: name of the column with the values we want to predict 
  #       plot_bool: if TRUE we plot the histogram and qqPlot of the residuals from Lasso  
  # Return: a list with rmse,smape scores and the fitted lasso model

  
  results_lasso <-  array(1:2)
  results_kriging <-  array(1:2)
  
  matrixCovariates <-  as.matrix(as.data.frame(train)[,1:5])
  
  matrixTarget <-  train[[what_to_predict]]
  
  testCovariates <-  as.matrix(as.data.frame(test)[,1:5])
  
  testCovariates <-  scale(testCovariates, center = apply(matrixCovariates,2,mean), scale = apply(matrixCovariates,2,sd))
  
  matrixCovariates <-  scale(matrixCovariates)
  
  
  cv <-  cv.glmnet(matrixCovariates, matrixTarget, alpha = 1, grouped = FALSE) 
  
  bestLambda <-  cv$lambda.min
  
  bestModel <-  glmnet(matrixCovariates, matrixTarget, alpha = 1, lambda = bestLambda,standardize = FALSE)
  
  residuals <-  matrixTarget - predict(bestModel, s=bestLambda, newx =matrixCovariates ) # calculate the residuals from lasso 
  
  if(plot_bool == TRUE){
    hist(residuals)
    qqPlot(residuals)
  }
  
  train$resid <-  residuals
  
  best_variogram <- autofitVariogram(resid ~ 1, train)
  
  residKriged <-  krige(resid ~ 1,locations = train,newdata = test,model = best_variogram$var_model,beta = 0)
  
  lasso_prediciton <-  predict(bestModel,s=bestLambda, newx = testCovariates)
  
  test$predi <-  lasso_prediciton + residKriged$var1.pred
  
  test_true <-  test[[what_to_predict]] 
  
  results_lasso[1] <-  Metrics::rmse( lasso_prediciton,test_true)
  results_lasso[2] <-  Metrics::smape( lasso_prediciton,test_true)  
  
  results_kriging[1] <-  Metrics::rmse( test$predi,test_true)
  results_kriging[2] <-  Metrics::smape( test$predi,test_true)  
  
  
  return(list(results_kriging, results_lasso,bestModel))
}

bayesianlinearKrigingPrediction = function(train, test, what_to_predict, plot_bool){
  
  # performs Kriging with external drift provided by a linear bayesian model
  # Args:
  #       train: training sf dataframe
  #       test: testing sf dataframe
  #       what_to_predict: name of the column with the values we want to predict 
  #       plot_bool: if TRUE we plot the histogram and qqPlot of the residuals from the linear model
  # Return: a list with rmse,smape scores and the fitted linear model
  
  
  results_bayesian <-  array(1:2)
  results_kriging <-  array(1:2)
  
  matrixCovariates <-  as.matrix(as.data.frame(train)[,1:5])
  
  matrixTarget <-  train[[what_to_predict]]
  
  testCovariates <-  as.matrix(as.data.frame(test)[,1:5])
  
  testCovariates <-  scale(testCovariates, center = apply(matrixCovariates,2,mean), scale = apply(matrixCovariates,2,sd))
  
  matrixCovariates <-  scale(matrixCovariates) %>% as.data.frame()
  
  linear_model <- stan_glm(matrixTarget ~., data = matrixCovariates, seed=42)
  
  residuals <-  matrixTarget - predict(linear_model, newx = matrixCovariates ) #
  
  print(plot(linear_model, plotfun = "trace"))
  print(summary(linear_model))
  
  if(plot_bool == TRUE){
    
    hist(residuals)
    qqPlot(residuals)
    
  }
  
  train$resid <-  residuals
  
  best_variogram <- autofitVariogram(resid ~ 1, train)
  
  residKriged <-  krige(resid ~ 1,locations = train,newdata = test,model = best_variogram$var_model,beta = 0)
  
  bayesian_prediciton <-  predict(linear_model, newdata = as.data.frame(testCovariates))
  
  test$predi <-  bayesian_prediciton + residKriged$var1.pred
  
  test_true <-  test[[what_to_predict]] 
  
  results_bayesian[1] <-  Metrics::rmse( bayesian_prediciton,test_true)
  results_bayesian[2] <-  Metrics::smape( bayesian_prediciton,test_true)  
  
  results_kriging[1] <-  Metrics::rmse( test$predi,test_true)
  results_kriging[2] <-  Metrics::smape( test$predi,test_true)  
  
  
  return(list(results_kriging, results_bayesian,linear_model))
  
}



# KRIGING #

kriging_data <-  read.xlsx(xlsxFile = "data\\kriging_data.xlsx", sheet = "All Years") 

miced_data <-  mice(kriging_data)
kriging_data <-  complete(miced_data) # impute missing data

kriging_data.sf <-  st_as_sf(kriging_data,coords = c('lon','lat'), crs=4326)
kriging_data.sf <-  st_transform(kriging_data.sf , crs = 3995)


kriging_data.sf <-  temporal_distancing(kriging_data.sf)

block_initial <- spatial_initial_split(kriging_data.sf,prop = 0.2, spatial_block_cv)

train_sf <-  training(block_initial)

test_sf <-  testing(block_initial)


krig_lasso_N1 <-  lassoKrigingPrediction(train_sf, test_sf, "N1", TRUE)
coef(krig_lasso_N1[[3]])

krig_lasso_N2 <-  lassoKrigingPrediction(train_sf, test_sf, "N2", TRUE)
coef(krig_lasso_N1[[3]])



krig_blm_N1 <-  bayesianlinearKrigingPrediction(train_sf, test_sf, "N1", TRUE)


krig_blm_N2 <-  bayesianlinearKrigingPrediction(train_sf, test_sf, "N2", TRUE)








