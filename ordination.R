setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/cleanplot.pca.R')

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_alt = zoo_community$Altitude


###### DCA ######

# perform DCA to determine which method to use

dca = decorana(zoo_spe, ira = 0) 
dca

zoo_env.f = zoo_env
zoo_env.f$DO = NULL
zoo_env.f$Conductivity = log(zoo_env$Conductivity)
zoo_env.f$Temperature = log(zoo_env$Temperature)
zoo_env.f$Depth = log(zoo_community$Depth)

shapiro.test(zoo_env$Depth)

###### RDA ######

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


p_max_explainable_var = RsquareAdj(full_rda)$r.squared / ( sum(spe_pca$CA$eig[1:3]) / sum(spe_pca$CA$eig[1:5]) )


##################### VARIATION PARTITIONING ##############################

partitioned = varpart(zoo_spe.trans, zoo_env.f, zoo_alt)

plot(partitioned,
     Xnames = c("Env", "3d"), # name the partitions
     bg = c("seagreen3", "mediumpurple"), alpha = 80, # colour the circles
     digits = 2, # only show 2 digits
     cex = 1.5)


#### TESTING SIGNIFICANCE OF PARTITIONING ####

anova.cca(rda(zoo_spe.trans, zoo_env.f), step = 9999)
anova.cca(rda(zoo_spe.trans, zoo_extra), step = 9999)
anova.cca(rda(zoo_spe.trans, zoo_env.f,zoo_extra), step = 9999)
anova.cca(rda(zoo_spe.trans,zoo_extra, zoo_env.f), step = 9999)






