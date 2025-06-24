setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
library(adespatial)


############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- get_community_data(zoo_yearly, 'Phylum')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_spat <- zoo_community[c('lat','lon','alt', 'Depth')]

zoo_env.z <- decostand(zoo_env, method = "standardize")
zoo_spe.hel <- decostand(zoo_spe, "hellinger")


################# PARTIAL RDA #############

distance_matrix = dist(zoo_spat)
moran = pcnm(distance_matrix)$vectors %>% as.data.frame()

informed_vars = zoo_env.z[c("Conductivity", "pH", "Temperature")]

rda(zoo_spe.hel, informed_vars, moran)

##################### VARIATION PARTITIONING ##############################

partitioned = varpart(zoo_spe.hel, informed_vars, moran)

plot(partitioned,
     Xnames = c("Chem", "Topo"), # name the partitions
     bg = c("seagreen3", "mediumpurple"), alpha = 80, # colour the circles
     digits = 2, # only show 2 digits
     cex = 1.5)


#### TESTING SIGNIFICANCE OF PARTITIONING ####

anova.cca(rda(zoo_spe.hel, informed_vars), step = 9999)
anova.cca(rda(zoo_spe.hel, moran), step = 9999)
anova.cca(rda(zoo_spe.hel, informed_vars,moran), step = 9999)
anova.cca(rda(zoo_spe.hel,moran, informed_vars), step = 9999)































