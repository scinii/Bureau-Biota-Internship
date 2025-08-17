setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
library(spdep)
library(adegraphics)
library(adespatial)

source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/plot.links.R')


############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_xy <- zoo_community[c('lon','lat')]

zoo_xy = st_as_sf(zoo_xy,coords = c('lon','lat'), crs=4326)

zoo_xy = st_transform(zoo_xy , crs = 3995)

zoo_xy$X = st_coordinates(zoo_xy)[,1]
zoo_xy$Y = st_coordinates(zoo_xy)[,2]
zoo_xy$geometry = NULL
zoo_xy = scale(zoo_xy, center = TRUE, scale = FALSE) %>% as_data_frame()


zoo_xyz = zoo_xy
zoo_xyz$z = zoo_community$Altitude


zoo_env.t = zoo_env
zoo_env.t$Conductivity = log(zoo_env$Conductivity)
zoo_env.t$Temperature = log(zoo_env$Temperature)
zoo_env.t$Depth = log(zoo_community$Depth)

most_common_lambda =  sensitivity_analysis(zoo_spe, zoo_env.t, c("Conductivity", "pH", "Temperature", "Depth"))

zoo_spe.trans = box_cox_trans(zoo_spe, most_common_lambda[1])


#### TREND SURFACE ANALYSIS ####

zoo_poly = poly(as.matrix(zoo_xyz), degree = 2, raw = FALSE)
colnames(zoo_poly) <-  c("X", "X2", "Y", "XY", "Y2", "Z","ZX", "ZY","Z2")
(zoo_rda_poly <- rda(zoo_spe.trans ~ ., data =as.data.frame(zoo_poly)))

(zoo_rda.fwd <- forward.sel(zoo_spe.trans, zoo_poly , alpha = 0.1, nperm = 9999, R2thresh = RsquareAdj(zoo_rda_poly)$r.squared))
(mite.trend.rda2 <- rda(zoo_spe.trans ~ .,
                        as.data.frame(zoo_poly)[ ,zoo_rda.fwd[ ,2]] ))
summary(mite.trend.rda2)
anova(mite.trend.rda2)
anova(mite.trend.rda2, by = "axis")


spe_pca = rda(zoo_spe.trans)
p_max_explainable_var = RsquareAdj(mite.trend.rda2)$r.squared / ( sum(spe_pca$CA$eig[1:3]) / sum(spe_pca$CA$eig[1:5]) )


mite.trend.fit <-
  scores(mite.trend.rda2,
         choices = 1:2,
         display = "lc",
         scaling = 1)
s.value(zoo_xyz, mite.trend.fit, symbol = "circle")

mite.rda2.axis1.env <- lm(mite.trend.fit[ ,1] ~ ., data = zoo_env.t)
shapiro.test(resid(mite.rda2.axis1.env))
summary(mite.rda2.axis1.env)

mite.rda2.axis2.env <- lm(mite.trend.fit[ ,2] ~ ., data = zoo_env.t)
shapiro.test(resid(mite.rda2.axis2.env))
summary(mite.rda2.axis2.env)

plot.links(zoo_xy, dist(zoo_xyz), thresh =1233)


#### CORRELOGRAMS ####
residuals = resid(lm(as.matrix(zoo_spe.trans) ~ XY + ZX + Y2, data = zoo_poly))
(mantelloz = mantel.correlog(dist(residuals), D.geo = dist(zoo_xyz), nperm = 9999, r.type="spearman"))
summary(mantelloz)
plot(mantelloz)


























