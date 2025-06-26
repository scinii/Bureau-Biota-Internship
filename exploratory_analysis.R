setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory
source('utils.R')




############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

split_rotifers_arthropodas(zoo_yearly, 'Genus')


zoo_dataframes <- get_community_data(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]

#zoo_2d <- zoo_community[c('lat','lon')]

#zoo_3d <- zoo_community[c('alt', 'Depth')]

zoo_env.z <- decostand(zoo_env, method = "standardize", MARGIN = 2)

zoo_spe.hel <- decostand(zoo_spe, "hellinger")


zoo_env.f = zoo_env
zoo_env.f$DO = NULL
zoo_env.f$Conductivity = log(zoo_env$Conductivity)
zoo_env.f$Temperature = log(zoo_env$Temperature)




#### MISSING DATA ####

# DONE
missing_data(zoo_yearly,'env')
missing_data(zoo_yearly, 'groups')



#### BOX PLOT ####

zoo_env.z %>% gather(key="EnvVar", value = "Val") %>%
  ggplot( aes(x=EnvVar, y=Val, fill=EnvVar)) + 
  geom_boxplot(alpha=0.6) + theme(legend.position="none") + 
  labs(x = "Environmental Variables", y="Standardized Value")

zoo_env.f %>% gather(key="EnvVar", value = "Val") %>%
  ggplot( aes(x=EnvVar, y=Val, fill=EnvVar)) + 
  geom_boxplot(alpha=0.6) + theme(legend.position="none") + 
  labs(x = "Environmental Variables", y="Standardized Value")




##### BUBBLE PLOTS ####

plot_bubble_map(zoo_community, "pH")
plot_bubble_map(zoo_community, "Conductivity")
plot_bubble_map(zoo_community, "Temperature")
plot_bubble_map(zoo_community, "DO")
plot_bubble_map(zoo_community, "Depth")
plot_bubble_map(zoo_community, "Altitude")



############ CORRELATIONS ############

corrplot(cor(zoo_env), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)


bray_curtis_diss = vegdist(log1p(zoo_spe), method = "bray")
hellinger_diss = dist(zoo_spe.hel)

corrplot(as.matrix(bray_curtis_diss), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)
corrplot(as.matrix(hellinger_diss), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)


div_2024 =  diversity_table(zoo_community[,c(1,8:27)])
#plot_bubble_map(div_2024, "N0")
plot_bubble_map(div_2024, "N1")
#plot_bubble_map(div_2024, "N2")
plot_bubble_map(div_2024, "E1") 
#plot_bubble_map(div_2024, "E2") # ~ Simpson


