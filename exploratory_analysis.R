setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')


#### VERY EXPLORATORY STUFF ####

#missing_data(data_yearly,'env')
#missing_data(data_yearly, 'groups')
#div_2024 =  diversity_table(data_yearly_comb[,c(1,8:27)])



############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_ext <- zoo_community[c('lat','lon', 'Drought')]




data_yearly_env.z <- decostand(data_yearly_env, method = "standardize")
data_yearly_conc.hel <- decostand(data_yearly_conc, "hellinger")


############ CORRELATIONS ############

bray_curtis_diss = vegdist(log1p(data_yearly_conc), method = "bray")

corrplot(as.matrix(bray_curtis_diss), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)

corrplot(cor(data_yearly_env.z), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)


####### ORDINATION ######

#dca = decorana(data_yearly_conc.hel, ira = 0) 

yearly_rda = rda(data_yearly_conc.hel ~., data=data_yearly_env.z)