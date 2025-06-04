library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)

# Set working directory
setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') 

numeric_variables <- c("Year","pH",	"DO",	"Conductivity",	"Temperature", "Depth",
                       "Drought", "Counts")

location_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")
lakes_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "counts") %>%
              select(-'Name',-'Taxa') %>%
              merge(location_data,by="Location")
lakes_data[numeric_variables] <- sapply(lakes_data[numeric_variables], as.numeric)
lakes_data$C100L = NULL


split_yearly_data <- function(df){
  
  wb <- createWorkbook()
  
  all_years <- c(2018,2019,2020,2021,2022,2023,2024)
  
  
  for(year in all_years){
    
    bool_idx <- df$Year == year
    
    yearly_data <- df[bool_idx,]
    
    addWorksheet(wb, paste("Year", year))
    
    writeData(wb, sheet = paste("Year", year), x = yearly_data)
      
  }
  
  saveWorkbook(wb, file = 'yearly_data.xlsx', overwrite = TRUE)
  
}






