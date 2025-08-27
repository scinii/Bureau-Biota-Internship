setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')

kriging_yearly_data <- function(){
  
  "This function takes the first two dataframes obtained from split_rotifers_arthropodas
   computes the N1 and N2 Hill's numbers and returns a dataframe with the following columns  
    lon, lat, pH, Conductivity, Temperature, Depth, Altitude, N1, N2, Altitude and year "
  
  all_years <- c("Year 2018","Year 2019","Year 2020","Year 2021","Year 2022","Year 2023","Year 2024")
  wb <- createWorkbook()
  
  combined_list <- list()
  
  for(year in all_years){
    
    zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = year)
    
    zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')
    
    zoo_community <- zoo_dataframes[[1]]
    
    zoo_spe <- zoo_dataframes[[2]]
    
    print(length(colnames(zoo_spe)))
    
    dt = diversity_table(zoo_spe, colnames(zoo_spe))
    
    
    data_kriging = zoo_community[,c(9,8,2,4:6,10)]
    data_kriging$N1 = dt$N1
    data_kriging$N2 = dt$N2
    data_kriging$Abundance = rowSums(zoo_spe)
    data_kriging$Year <- gsub("Year ", "", year)
    
    addWorksheet(wb, year)
    writeData(wb, sheet = year, x = data_kriging)
    saveWorkbook(wb, file = 'kriging_data.xlsx', overwrite = TRUE)
    combined_list[[year]] <- data_kriging
  }
  combined_all_years <- bind_rows(combined_list)
  
  addWorksheet(wb, "All Years")
  writeData(wb, sheet = "All Years", x = combined_all_years)
  saveWorkbook(wb, file = 'kriging_data.xlsx', overwrite = TRUE)
  
}














