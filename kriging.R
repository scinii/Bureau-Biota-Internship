setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')

library(glmnet) # Lasso Regression
library(dismo) # for k-fold cross validation
library(sf) # for converting coordinates
library(gstat) # for kriging
library(Metrics) # for performance scores

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

dt = diversity_table(zoo_spe)

data_kriging = zoo_community[,c(8,9,2:6,10)]
data_kriging$abundance = dt$N2
data_kriging$eveness = dt$E2

