setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
library(adespatial)
library(vegan3d)

### VERY EXPLORATORY STUFF ####

#missing_data(data_yearly,'env')
#missing_data(data_yearly, 'groups')
#div_2024 =  diversity_table(data_yearly_comb[,c(1,8:27)])

############ CORRELATIONS ############

#bray_curtis_diss = vegdist(log1p(data_yearly_conc), method = "bray")

#corrplot(as.matrix(bray_curtis_diss), is.corr = FALSE, method = 'color',
#         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)


####### ORDINATION ######
#dca = decorana(data_yearly_conc.hel, ira = 0) 

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

corrplot(cor(zoo_env.z), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)

###### RDA ######

#FULL
full_rda = rda(zoo_spe.hel ~ pH + Conductivity + Temperature + DO, data=zoo_env.z)
summary(full_rda)
anova.cca(full_rda, step=9999)


# CHRISTOPH's MODEL

cristoph_rda = rda(zoo_spe.hel ~ pH + Conductivity + Temperature, data=zoo_env.z)
summary(cristoph_rda)
anova.cca(cristoph_rda, step=9999)
anova.cca(cristoph_rda, step=9999,by="term" )
anova.cca(cristoph_rda, step=9999,by="axis")
ordiplot(cristoph_rda, scaling = 1, type = "text", main = "Cristoph RDA - Scaling 1")
ordiplot(cristoph_rda, scaling = 2, type = "text", main = "Cristoph RDA - Scaling 2")
sel_cristoph.fs = forward.sel(Y = zoo_spe.hel, X = zoo_env.z , adjR2thresh =  RsquareAdj(full_rda)$r.squared, nperm = 49999, alpha = 0.1)



# INFORMED MODEL

informed_rda = rda(zoo_spe.hel ~ pH + Conductivity, data=zoo_env.z)
summary(informed_rda) # 31.88%
anova.cca(informed_rda, step=9999)

triplot = function(){
  
  plot(informed_rda,
       scaling = 1,
       display = c("sp", "lc", "cn"),
       main = "Informed RDA - Scaling 1"
  )
  spe.sc1 <-
    scores(informed_rda,
           choices = 1:2,
           scaling = 1,
           display = "sp"
    )
  arrows(0, 0,
         spe.sc1[, 1] * 0.9,
         spe.sc1[, 2] * 0.9,
         length = 0,
         lty = 1,
         col = "red"
  )
  
  plot(informed_rda,
       display = c("sp", "lc", "cn"),
       main = "Informed RDA - Scaling 2"
  )
  spe.sc2 <-
    scores(informed_rda,
           choices = 1:2,
           display = "sp"
    )
  arrows(0, 0,
         spe.sc2[, 1] * 0.9,
         spe.sc2[, 2] * 0.9,
         length = 0,
         lty = 1,
         col = "red"
  )
  
}

informed_pca = rda(zoo_spe.hel) # 57.35%

cor.test (~ pH + Conductivity, data = zoo_env.z)



################# PARTIAL RDA #############


rda(zoo_spe.hel, informed_vars, moran)

##################### VARIATION PARTITIONING ##############################

informed_vars = zoo_env.z[c("Conductivity", "pH")]


distance_matrix = dist(zoo_spat)
moran = pcnm(distance_matrix)$vectors %>% as.data.frame()


partitioned = varpart(zoo_spe.hel, informed_vars, moran)

plot(partitioned,
     Xnames = c("Chem", "Topo"), # name the partitions
     bg = c("seagreen3", "mediumpurple"), alpha = 80, # colour the circles
     digits = 2, # only show 2 digits
     cex = 1.5)

anova.cca(rda(zoo_spe.hel, informed_vars), step = 9999)
anova.cca(rda(zoo_spe.hel, moran), step = 9999)
anova.cca(rda(zoo_spe.hel, informed_vars,moran), step = 9999)
anova.cca(rda(zoo_spe.hel,moran, informed_vars), step = 9999)