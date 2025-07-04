setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')



############ GET YEARLY DATA ############

which_year = "Year 2020"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]




#### DATA TRANSFORMATION  ####

zoo_env.f = zoo_env

zoo_env.f$DO = NULL

zoo_env.f$Conductivity = log(zoo_env$Conductivity)

zoo_env.f$Temperature = log(zoo_env$Temperature)


sensitivity_analysis(zoo_spe, zoo_env.f)

max_var_box_cox(zoo_spe, zoo_env.f, 0.3, TRUE)

zoo_spe.trans = box_cox_trans(zoo_spe, 0.5)


#### MISSING DATA ####


missing_data(zoo_yearly,'env')
missing_data(zoo_yearly, 'groups')


#### BOX PLOT ####

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

corrplot(cor(zoo_env.f), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)


corrplot(as.matrix(vegdist(zoo_spe, method = "bray")), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)




div_2024 =  diversity_table(zoo_community[,c(1,8,9,11:15)])
#plot_bubble_map(div_2024, "N0")
plot_bubble_map(div_2024, "N1")
#plot_bubble_map(div_2024, "N2")
plot_bubble_map(div_2024, "E1") 
#plot_bubble_map(div_2024, "E2") # ~ Simpson


