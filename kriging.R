setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

library(openxlsx) # Open Excel files
library(glmnet) # Lasso Regression
library(dplyr) # data manipulation
library(gstat) # for kriging
library(Metrics) # for performance scores
library(tidyverse) # for manipulating the data frame
library(dismo) # for k-fold cross validation
library(sf) # for converting coordinates
set.seed(42) # for reproducibility 


kriging_data = read.xlsx(xlsxFile = "kriging_data.xlsx", sheet = "kriging_data_24")

kriging_data.sf = st_as_sf(kriging_data,coords = c('lon','lat'), crs=4326)
kriging_data.sf = st_transform(kriging_data.sf , crs = 3995)
