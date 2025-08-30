library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)

# Set working directory
setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') 
source('utils.R')



split_yearly_data <- function(){
  
  "This function split the data by year and saves each yearly data
  in a separate excel sheet (but same file)"
  
  numeric_variables <- c("Year","pH",	"DO",	"Conductivity",	"Temperature", "Depth",
                         "Drought", "Counts")
  
  location_data <- read.xlsx(xlsxFile = "data\\data_lakes.xlsx", sheet = "locations")
  lakes_data <- read.xlsx(xlsxFile = "data\\data_lakes.xlsx", sheet = "counts") %>%
    dplyr::select(-Name, -Taxa) %>%
    merge(location_data,by="Location")
  lakes_data[numeric_variables] <- sapply(lakes_data[numeric_variables], as.numeric)
  lakes_data$C100L = NULL
  
  df <- lakes_data
  
  
  wb <- createWorkbook()
  
  all_years <- c(2018,2019,2020,2021,2022,2023,2024)
  
  
  for(year in all_years){
    
    bool_idx <- df$Year == year
    
    yearly_data <- df[bool_idx,]
    
    addWorksheet(wb, paste("Year", year))
    
    writeData(wb, sheet = paste("Year", year), x = yearly_data)
      
  }
  
  saveWorkbook(wb, file = 'data\\yearly_data.xlsx', overwrite = TRUE)
  
}

kriging_yearly_data <- function(){
  
  "This function takes the first two dataframes obtained from split_rotifers_arthropodas
   computes the N1 and N2 Hill's numbers and returns a dataframe with the following columns  
    lon, lat, pH, Conductivity, Temperature, Depth, Altitude, N1, N2, Altitude and year "
  
  all_years <- c("Year 2018","Year 2019","Year 2020","Year 2021","Year 2022","Year 2023","Year 2024")
  wb <- createWorkbook()
  
  combined_list <- list()
  
  for(year in all_years){
    
    zoo_yearly <- read.xlsx(xlsxFile = "data\\yearly_data.xlsx", sheet = year)
    
    zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')
    
    zoo_community <- zoo_dataframes[[1]]
    
    zoo_spe <- zoo_dataframes[[2]]
    
    dt <-  diversity_table(zoo_spe, colnames(zoo_spe))
    
    
    data_kriging <-  zoo_community[,c(9,8,2,4:6,10)]
    data_kriging$N1 <-  dt$N1
    data_kriging$N2 <-  dt$N2
    data_kriging$Abundance <-  rowSums(zoo_spe) 
    data_kriging$Year <- gsub("Year ", "", year)
    
    addWorksheet(wb, year)
    writeData(wb, sheet <-  year, x = data_kriging)
    saveWorkbook(wb, file <-  'data\\kriging_data.xlsx', overwrite = TRUE)
    combined_list[[year]] <- data_kriging
  }
  combined_all_years <- bind_rows(combined_list)
  
  addWorksheet(wb, "All Years")
  writeData(wb, sheet <-  "All Years", x =  combined_all_years)
  saveWorkbook(wb, file <-  'data\\kriging_data.xlsx', overwrite =  TRUE)
  
}

