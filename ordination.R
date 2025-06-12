setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
library(adespatial)

#dca = decorana(data_yearly_conc.hel, ira = 0) 

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_env.z <- decostand(zoo_env, method = "standardize")
zoo_spe.hel <- decostand(zoo_spe, "hellinger")


###### RDA ######

#FULL
full_rda = rda(zoo_spe.hel ~ pH + Conductivity + Temperature + DO, data=zoo_env.z)
summary(full_rda)
anova.cca(full_rda, step=9999)


# CHRISTOPH's MODEL

cristoph_rda = rda(zoo_spe.hel ~ pH  + Temperature + Conductivity, data=zoo_env.z)
summary(cristoph_rda)
anova.cca(cristoph_rda, step=9999)
anova.cca(cristoph_rda, step=9999,by="term" )
anova.cca(cristoph_rda, step=9999,by="axis")
sel_cristoph.fs = forward.sel(Y = zoo_spe.hel, X = zoo_env.z , adjR2thresh =  RsquareAdj(full_rda)$r.squared, nperm = 49999, alpha = 0.2)

plot_rda(cristoph_rda, 1)
plot_rda(cristoph_rda, 2)







informed_pca = rda(zoo_spe.hel) # 57.35%

cor.test(~ pH + Conductivity, data = zoo_env.z)
cor.test(~ pH + Temperature, data = zoo_env.z)
cor.test(~ Temperature + Conductivity, data = zoo_env.z)




