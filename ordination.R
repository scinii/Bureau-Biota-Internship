setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')


############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]


###### DCA ######

# perform DCA to determine which method to use

dca = decorana(zoo_spe, ira = 0) 

zoo_env.f = zoo_env
zoo_env.f$DO = NULL
zoo_env.f$Conductivity = log(zoo_env$Conductivity)
zoo_env.f$Temperature = log(zoo_env$Temperature)




###### RDA ######

sensitivity_analysis(zoo_spe, zoo_env.f)
max_var_box_cox(zoo_spe, zoo_env.f, 0.3, TRUE)

zoo_spe.trans = box_cox_trans(zoo_spe, 0.19)


# FULL MODEL
full_rda = rda(zoo_spe.trans ~ Conductivity + pH  + Temperature, data=zoo_env.f)

summary(full_rda)
anova.cca(full_rda, step=9999)
anova.cca(full_rda, step=9999,by="term" )
anova.cca(full_rda, step=9999,by="axis")

# sel_infromed.fs = forward.sel(Y = zoo_spe.hel, X = zoo_env.f , adjR2thresh =  RsquareAdj(full_rda)$r.squared, nperm = 49999, alpha = 0.2)


plot_rda(full_rda)

spe_pca = rda(zoo_spe.trans)


p_max_explainable_var = RsquareAdj(full_rda)$r.squared / ( sum(spe_pca$CA$eig[1:3]) / sum(spe_pca$CA$eig[1:5]) )









