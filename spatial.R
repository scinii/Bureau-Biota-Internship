setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')
source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/plot.links.R')
library(spdep)
library(adegraphics)
library(adespatial)


############ GET YEARLY DATA ############

which_year <-  "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "data\\yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

zoo_xy <- zoo_community[c('lon','lat')]


# convert coordinates to have them in meters

zoo_xy <-  st_as_sf(zoo_xy,coords = c('lon','lat'), crs=4326)

zoo_xy <-  st_transform(zoo_xy , crs = 3995)

zoo_xy$X <-  st_coordinates(zoo_xy)[,1]
zoo_xy$Y <-  st_coordinates(zoo_xy)[,2]
zoo_xy$geometry <-  NULL
zoo_xy <-  scale(zoo_xy, center = TRUE, scale = FALSE) %>% as_data_frame()

zoo_xyz <-  zoo_xy
zoo_xyz$z <-  zoo_community$Altitude


# Data transformation

zoo_env.t <-  decostand(zoo_env, "standardize")

zoo_spe.trans <-  box_cox_trans(zoo_spe, 1)


#### TREND SURFACE ANALYSIS ####

zoo_poly <-  poly(as.matrix(zoo_xyz), degree = 2, raw = FALSE)
colnames(zoo_poly) <-  c("X", "X2", "Y", "XY", "Y2", "Z","ZX", "ZY","Z2")
zoo_rda_poly <- rda(zoo_spe.trans ~ ., data =as.data.frame(zoo_poly))

zoo_rda.fwd <- forward.sel(zoo_spe.trans, zoo_poly , alpha = 0.1, nperm = 9999, R2thresh = RsquareAdj(zoo_rda_poly)$r.squared)

zoo_trend <- rda(zoo_spe.trans ~ .,as.data.frame(zoo_poly)[ ,zoo_rda.fwd[ ,2]] )

summary(zoo_trend)
anova(zoo_trend)
anova(zoo_trend, by = "axis")


spe_pca <-  rda(zoo_spe.trans)
p_max_explainable_var <-  RsquareAdj(zoo_trend)$r.squared / ( sum(spe_pca$CA$eig[1:3]) / sum(spe_pca$CA$eig[1:5]) )


trend_scores <-  scores(zoo_trend , choices = 1:2, display = "lc", scaling = 1)

s.value(zoo_xyz, trend_scores, symbol = "circle")

scores_RDA1_env <- lm(trend_scores[ ,1] ~ ., data = zoo_env.t)
shapiro.test(resid(scores_RDA1_env))
summary(scores_RDA1_env)

scores_RDA2_env <- lm(trend_scores[ ,2] ~ ., data = zoo_env.t)
shapiro.test(resid(scores_RDA2_env))
summary(scores_RDA2_env)


plot.links(zoo_xy, dist(zoo_xyz), thresh =1233)


# First detrend and compute the correlogram

residuals <-  resid(lm(as.matrix(zoo_spe.trans) ~ XY + ZX + Y2, data = zoo_poly))
(correlogram <-  mantel.correlog(dist(residuals), D.geo = dist(zoo_xyz), nperm = 9999, r.type="spearman"))
summary(correlogram)
plot(correlogram)


























