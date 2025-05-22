library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)
library(naniar) # visualize missing data
library(sf) # Spatial Dataframes

# Set working directory
setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') 


variables_of_interest <- c("Location", "Year",	"pH",	"DO",	"Conductivity","Temperature",
                           "Depth",	"Drought",	"Name",	"Taxa","Concentration")
numeric_variables <- c("Year","pH",	"DO",	"Conductivity",	"Temperature", "Depth",
                       "Drought", "Concentration")

location_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")
lakes_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "counts")[variables_of_interest]


lakes_data[numeric_variables] <- sapply(lakes_data[numeric_variables], as.numeric)
lakes_data <- merge(lakes_data,location_data,by="Location") |> st_as_sf(coords = c("lon", "lat"), crs = 3995)

coord <- as.data.frame(st_coordinates(lakes_data)) 
lakes_data$lon <- coord$X
lakes_data$lat <- coord$Y
lakes_data$geometry <- NULL


missing_data <- function(){
  
  for(i in c("2021","2022","2023","2024")){
    
    df = get_data(lakes_data,i,)
    print(df)
    print(unique(df$Location))
    print(nrow(df))
    df = df[c("pH","DO","Conductivity","Temperature","Depth")]
    
    if(n_var_miss(df) > 0){
      missing2 = vis_miss(df) + ggtitle(paste("Missing data for year", i)) +
        theme(plot.title = element_text(hjust = 0.5))
      print(missing2)
    }
  }
}

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






