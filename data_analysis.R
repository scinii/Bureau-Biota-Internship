library(openxlsx)
library(dplyr)
library(tidyr)

#### keep concentration rather than counts as the volume is different ####

setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

variables_of_interest = c("Location", "Year",	"pH",	"DO",	"Conductivity",	"Temperature",	"Depth",	"Drought",	"Name",	"Taxa",	"Count", "Concentration")

full_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "counts")[1:604,]

samples_data <- full_data[variables_of_interest]

all_years_codes <- unique(samples_data$Year)
all_lakes_codes <- unique(samples_data$Location)

code_in_list <- function(list_codes, sample_code, which_code){
  
  # TODO: specify what the codes can be !!!! 
  # Description: This function checks if there is an element of list_codes 
  #              (a string) that is contained in the string sample_code
  # Args:
  #       list_codes: list of codes
  #       sample_code: reference string for checking
  #       which_code: "loc" if we are identifying the Lake.
  #                   "year" if we are identifying the Year
  # Return: TRUE if there is such element. FALSE otherwise
  
  in_list <- FALSE
  
  if(which_code == "loc"){
    
    for(i in 1:length(list_codes)){
      
      # check if the i-th element is contained in the sample_code string.
      # if yes then set in_list to TRUE and break as we only need one element
      # to be in the sample_code
      
      if( list_codes[i] == sample_code){
        in_list <- TRUE
        break
      }
    }
  }
  
  else if( which_code == "year"){
    
    for(i in 1:length(list_codes)){
      
      # same logic as the previous loop.
      
      if(grepl(list_codes[i],sample_code) == TRUE){
        in_list <- TRUE
        break
      }
    }
  }

  return(in_list)
  
}

get_data = function(lake_codes = all_lakes_codes,year_codes = all_years_codes){
  
  # Description: This function slice the dataframe according to the 
  #              lakes and years we want.
  # Args:
  #       lake_codes: list of codes that specify the lakes
  #       year_codes: list of codes that specify the years
  # Return: the dataframe we wanted
  
  # LOCATION SELECTION
  
  code_loc <- c()
  
  for(i in 1:length(samples_data$Location)){
    
    if( code_in_list(lake_codes, samples_data$Location[i], "loc") == TRUE){
      code_loc <- append(code_loc, i)
    }
  }
  
  data_code <- samples_data[code_loc,]
  
  # YEAR SELECTION
  
  year_loc <- c()
  
  for(i in 1:length(data_code$Year)){
    
    if( code_in_list(year_codes, data_code$Year[i], "year") == TRUE){
      year_loc <- append(year_loc, i)
    }
  }
  data_code <- data_code[year_loc,]
  
  return(data_code)
}

cca_data <- function(year_code){
  
  # Description: This function provides the two dataframes that are used in the 
  #              cca function of the vegan package
  # Args:
  #       year_code: which year you want to consider
  # Return: a list of two dataframe. The first one contains the counts for the
  #         the different Taxas. The second one contains the value relative to
  #         the environmental variables.
  
  year_data <- get_data(,year_code)
  
  
  taxa_df <- year_data|> 
            group_by(Location, pH, DO, Conductivity, Temperature, Depth, Taxa) |>
            summarise(
              Concentration = sum(Concentration),
              .groups = "drop"
            )  |> 
            pivot_wider(names_from = Taxa, values_from = Concentration, values_fill = 0)
  
  cca_df <- taxa_df[c("Location", "Rotifera", "Cladocera","Copepoda")]
  

  env_df <- subset(taxa_df, select = -c(Rotifera, Cladocera,Copepoda))
  env_df <- env_df[!duplicated(env_df),]

    
  return(list(cca_df,env_df))
}
