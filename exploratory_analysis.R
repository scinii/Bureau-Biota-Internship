setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')

############ GET YEARLY DATA ############

which_year <-  "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]


###### DATA TRANSFORMATION  ######

## ENVIRONMENTAL VARS ##

zoo_env.t <-  decostand(zoo_env, "standardize")

## SPECIES VARS ##

max_var_box_cox(zoo_spe, zoo_env.t, c("Conductivity", "pH", "Temperature", 'Depth'), NA, TRUE)

most_common_lambda <- sensitivity_analysis(zoo_spe, zoo_env.t, c("Conductivity", "pH", "Temperature", 'Depth'))

zoo_spe.trans <-  box_cox_trans(zoo_spe, most_common_lambda[1])


###### PLOTS ######

missing_data(zoo_yearly,'env')
missing_data(zoo_yearly, 'groups')


plot_histogram(zoo_env['Conductivity'])
plot_histogram(zoo_env['pH'])
plot_histogram(zoo_env['Temperature'])
plot_histogram(zoo_env['Depth'])
plot_histogram(zoo_community['Altitude'])


plot_bubble_map(zoo_community, "pH")
plot_bubble_map(zoo_community, "Conductivity")
plot_bubble_map(zoo_community, "Temperature")
plot_bubble_map(zoo_community, "Depth")
plot_bubble_map(zoo_community, "Altitude")


corrplot(cor(zoo_env.t), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)





