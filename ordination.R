setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')

############ GET YEARLY DATA ############

which_year <-  "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]


# perform DCA to determine which method to use

dca <-  decorana(zoo_spe, ira = 0) 
dca


###### TRANSFORMATIONS ######

zoo_env.t <-  decostand(zoo_env, "standardize")

zoo_spe.trans <-  box_cox_trans(zoo_spe, 1)


# FULL MODEL
full_rda <-  rda(zoo_spe.trans ~ Conductivity + pH  + Temperature + Depth, data=zoo_env.t)

summary(full_rda)
anova.cca(full_rda, step=9999)
anova.cca(full_rda, step=9999,by="axis")
anova.cca(full_rda, step=9999,by="term")


# full_rda

plot_ordination(full_rda, "rda", 1)
plot_ordination(full_rda, "rda", 2)

spe_pca <-  rda(zoo_spe.trans)

plot_ordination(spe_pca, "pca", 1)
plot_ordination(spe_pca, "pca", 2)


p_max_explainable_var <-  RsquareAdj(full_rda)$r.squared / ( sum(spe_pca$CA$eig[1:4]) / sum(spe_pca$CA$eig[1:5]) )







