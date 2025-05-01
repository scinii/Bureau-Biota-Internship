library(openxlsx)

setwd('C:\\Users\\rober\\Desktop\\Internship') # set working directory

samples_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "counts")[1:604,]

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


get_data = function(lake_codes = all_lakes_codes ,year_codes = all_years_codes){
  
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
