setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')


############ GET YEARLY DATA ############
which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

dt = diversity_table(zoo_spe)

data_kriging = zoo_community[,c(9,8,2,4:6,10)]
data_kriging$Altitude = decostand(data_kriging$Altitude, method = "standardize")[1:13]
data_kriging$Conductivity = log(data_kriging$Conductivity)
data_kriging$Temperature = log(data_kriging$Temperature)
data_kriging$Depth = log(data_kriging$Depth)
data_kriging$abundance = dt$N2
data_kriging$eveness = dt$E2


wb <- createWorkbook()
addWorksheet(wb, 'kriging_data_24')
writeData(wb, sheet = 'kriging_data_24', x = data_kriging)
saveWorkbook(wb, file = 'kriging_data.xlsx', overwrite = TRUE)

