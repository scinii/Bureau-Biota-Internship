setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
source("plot.links.R")
library(spdep)
library(adegraphics)

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)


zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_xy <- zoo_community[c('lat','lon')]

zoo_xy = st_as_sf(zoo_xy,coords = c('lon','lat'), crs=4326)

zoo_xy = st_transform(zoo_xy , crs = 3995)

zoo_xy$X = st_coordinates(zoo_xy)[,1]
zoo_xy$Y = st_coordinates(zoo_xy)[,2]
zoo_xy$geometry = NULL
zoo_xy = scale(zoo_xy, center = TRUE, scale = FALSE) %>% as_data_frame()


zoo_xyz = zoo_xy
zoo_xyz$z = zoo_community$Altitude


zoo_env.f = zoo_env
zoo_env.f$DO = NULL
zoo_env.f$Conductivity = log(zoo_env$Conductivity)
zoo_env.f$Temperature = log(zoo_env$Temperature)


most_common_lambda =  sensitivity_analysis(zoo_spe, zoo_env.f, c("Conductivity", "pH", "Temperature"))

zoo_spe.trans = box_cox_trans(zoo_spe, most_common_lambda)



#### CORRELOGRAMS ####

plot.links(zoo_xy, dist(zoo_xy),thresh = 1233.4)
(mantelloz = mantel.correlog(vegdist(zoo_spe, method = "bray"), D.geo = dist(zoo_xy), nperm = 9999, r.type="spearman"))
summary(mantelloz)
plot(mantelloz)


#### TREND SURFACE ANALYSIS ####

zoo_poly = poly(as.matrix(zoo_xyz), degree = 2)
colnames(zoo_poly) <-  c("X", "X2", "Y", "XY", "Y2", "Z","ZX", "ZY","Z2")
(zoo_rda_poly <- rda(zoo_spe.trans ~ ., data =as.data.frame(zoo_poly)))


(zoo_rda.fwd <- forward.sel(zoo_spe.trans, zoo_poly , alpha = 0.08, nperm = 9999, R2thresh = RsquareAdj(zoo_rda_poly)$r.squared))
(mite.trend.rda2 <- rda(zoo_spe.trans ~ XY + Z,
                        data = as.data.frame(zoo_poly)))
anova(mite.trend.rda2)
anova(mite.trend.rda2, by = "axis")


mite.trend.fit <-
  scores(mite.trend.rda2,
         choices = 1,
         display = "lc",
         scaling = 1)
s.value(zoo_xyz, mite.trend.fit, symbol = "circle")
mite.rda2.axis1.env <- lm(mite.trend.fit[ ,1] ~ ., data = zoo_env.f)
shapiro.test(resid(mite.rda2.axis1.env))
summary(mite.rda2.axis1.env)































