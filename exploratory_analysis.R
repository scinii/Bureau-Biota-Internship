setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory
source('utils.R')
library(memisc)

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_2d <- zoo_community[c('lat','lon')]

zoo_3d <- zoo_community[c('alt', 'Depth')]

zoo_env.z <- decostand(zoo_env, method = "standardize")
zoo_spe.hel <- decostand(zoo_spe, "hellinger")



##### BUBBLE PLOTS ####

plot_bubble_map(zoo_community, "pH")
plot_bubble_map(zoo_community, "Conductivity")
plot_bubble_map(zoo_community, "Temperature")
plot_bubble_map(zoo_community, "DO")




############ CORRELATIONS ############

#bray_curtis_diss = vegdist(log1p(data_yearly_conc), method = "bray")

#corrplot(as.matrix(bray_curtis_diss), is.corr = FALSE, method = 'color',
#         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)







#missing_data(data_yearly,'env')
#missing_data(data_yearly, 'groups')
#div_2024 =  diversity_table(data_yearly_comb[,c(1,8:27)])

corrplot(cor(zoo_env.z), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)

