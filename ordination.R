setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
library(adespatial)
library(car)


############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_env.z <- decostand(zoo_env, method = "standardize")
zoo_spe.hel <- decostand(zoo_spe, "hellinger")



###### DCA ######

# perform DCA to determine which method to use

dca = decorana(zoo_spe.hel, ira = 0) 

# for 2024 the axis length is 3.8019. This implies that we cannot make a clear cut
# between unimodal and linear methods. We will thus perform both




###### CCA ######

full_cca = cca(zoo_spe.hel ~ pH + Conductivity + Temperature + DO, data=zoo_env.z)
anova.cca(full_cca, step=9999)


plot(full_cca, scaling = 1, display = c("sp","lc","cn"), main = "CCA - scaling 1")
plot(full_cca, scaling = 2, display = c("sp","lc","cn"), main = "CCA - scaling 2")

plot(full_cca, scaling = 1, display = c("lc","cn"), main = "CCA - scaling 1")
plot(full_cca, scaling = 2, display = c("sp","cn"), main = "CCA - scaling 2")

informed_cca = cca(zoo_spe.hel ~ pH + Conductivity + Temperature, data=zoo_env.z)
anova.cca(informed_cca, step=9999)

plot(informed_cca, scaling = 1, display = c("sp","lc","cn"), main = "CCA - scaling 1")
plot(informed_cca, scaling = 2, display = c("sp","lc","cn"), main = "CCA - scaling 2")



###### RDA ######


# FULL MODEL
full_rda = rda(zoo_spe.hel ~ pH + Conductivity + Temperature + DO, data=zoo_env.z)
summary(full_rda)
anova.cca(full_rda, step=9999)
anova.cca(full_rda, step=9999,by="term" )
anova.cca(full_rda, step=9999,by="axis")


# INFORMED MODEL

infromed_rda = rda(zoo_spe.hel ~ pH  + Temperature + Conductivity, data=zoo_env.z)
infromed_pca = rda(zoo_spe.hel)
summary(infromed_rda)
p_max_explainable_var = RsquareAdj(infromed_rda)$r.squared / sum(infromed_pca$CA$eig[1:3])



anova.cca(infromed_rda, step=9999)
anova.cca(infromed_rda, step=9999,by="term" )
anova.cca(infromed_rda, step=9999,by="axis")
sel_infromed.fs = forward.sel(Y = zoo_spe.hel, X = zoo_env.z , adjR2thresh =  RsquareAdj(full_rda)$r.squared, nperm = 49999, alpha = 0.2)



cor.test(~ pH + Conductivity, data = zoo_env.z)
cor.test(~ pH + Temperature, data = zoo_env.z)
cor.test(~ pH + DO, data = zoo_env.z)
cor.test(~ Temperature + Conductivity, data = zoo_env.z)



plot_rda(infromed_rda, 1)
plot_rda(infromed_rda, 2)





