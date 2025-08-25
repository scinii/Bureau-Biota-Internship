setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_alt = zoo_community$Altitude


# perform DCA to determine which method to use

dca = decorana(zoo_spe, ira = 0) 
dca


###### TRANSFORMATIONS ######


zoo_env.t = zoo_env
zoo_env.t$DO = NULL
zoo_env.t$Conductivity = log(zoo_env$Conductivity)
zoo_env.t$Temperature = log(zoo_env$Temperature)
zoo_env.t$Depth = log(zoo_community$Depth)


max_var_box_cox(zoo_spe, zoo_env.f, c("Conductivity", "pH", "Temperature", 'Depth'), NA, TRUE)

most_common_lambda =  sensitivity_analysis(zoo_spe, zoo_env.f, c("Conductivity", "pH", "Temperature", 'Depth'))

zoo_spe.trans = box_cox_trans(zoo_spe, most_common_lambda[1])


# FULL MODEL
full_rda = rda(zoo_spe.trans ~ Conductivity + pH  + Temperature + Depth, data=zoo_env.f)

summary(full_rda)
anova.cca(full_rda, step=9999)
anova.cca(full_rda, step=9999,by="term" )
anova.cca(full_rda, step=9999,by="axis")


# full_rda

plot_ordination(full_rda, "rda", 1)
plot_ordination(full_rda, "rda", 2)

spe_pca = rda(zoo_spe.trans)

plot_ordination(spe_pca, "pca", 1)
plot_ordination(spe_pca, "pca", 2)


p_max_explainable_var = RsquareAdj(full_rda)$r.squared / ( sum(spe_pca$CA$eig[1:4]) / sum(spe_pca$CA$eig[1:5]) )







