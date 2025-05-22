library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)
library(vegan) # Perform CCA
library(sf) # Spatial Dataframes
library(ggplot2) 
library(corrplot) # Make Correlation Plots
library(usdm) # Package to calculate Variance Inflation Factor
library(naniar) # visualize missing data

# Set working directory
setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') 


cca_plot <- function(df,year_code, name_or_taxa, rhs_formula_string){
  
  # Description: This function performs cca and make the ordination plot
  #              
  # Args:
  #       year_code: which year you want to consider
  #       rhs_formula_string: the right hand side of the formula (i.e. which
  #       environmental variables you want to use)
  # Return: cca object of the vegan package.
  
  data <- cca_data(df,year_code,name_or_taxa)
  data_non_env <- data[[1]]
  data_env <- data[[2]]
  
  formula_cca <- as.formula(paste("data_non_env ~", rhs_formula_string))
  
  analysis <- cca(formula_cca, data = data_env)
  
  plot(analysis)
  return(analysis)
}


