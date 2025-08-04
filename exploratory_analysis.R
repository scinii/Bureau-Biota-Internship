setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

source('utils.R')

############ GET YEARLY DATA ############

which_year = "Year 2024"

zoo_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

zoo_dataframes <- split_rotifers_arthropodas(zoo_yearly, 'Genus')

zoo_community <- zoo_dataframes[[1]]

zoo_spe <- zoo_dataframes[[2]]

zoo_env <- zoo_dataframes[[3]]


#### MISSING DATA ####

missing_data(zoo_yearly,'env')
missing_data(zoo_yearly, 'groups')


#### DATA TRANSFORMATION  ####

## ENVIRONMENTAL VARS ##

zoo_env.t = zoo_env
zoo_env.t$Conductivity = log(zoo_env$Conductivity)
zoo_env.t$Temperature = log(zoo_env$Temperature)
zoo_env.t$Depth = log(zoo_community$Depth)

plot_histogram <- function(df){
  
  col_name = colnames(df)
  
  plt1 <- ggplot(df,aes(x="", y = .data[[col_name]]  )) +
    geom_boxplot(fill = "lightblue", color = "black") + 
    coord_flip() +
    theme_classic() +
    xlab("") +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  plt2 <- ggplot(df) +
    geom_histogram(aes(x = .data[[col_name]]),
                   position = "identity", 
                   fill = "lightblue", color = "black") +
    ylab("Frequency") +
    theme_classic()
  
  egg::ggarrange(plt2, plt1, heights = 2:1)
}


shapiro.test(zoo_env$pH)
plot_histogram(zoo_env['pH'])
plot_histogram(zoo_env.t['pH'])
shapiro.test(zoo_env.t$pH)

shapiro.test(zoo_env$Conductivity)
plot_histogram(zoo_env['Conductivity'])
plot_histogram(zoo_env.t['Conductivity'])
shapiro.test(zoo_env.t$Conductivity)

shapiro.test(zoo_env$Temperature)
plot_histogram(zoo_env['Temperature'])
plot_histogram(zoo_env.t['Temperature'])
shapiro.test(zoo_env.t$Temperature)


shapiro.test(zoo_env$Depth)
plot_histogram(zoo_env['Depth'])
plot_histogram(zoo_env.t['Depth'])
shapiro.test(zoo_env.t$Depth)

## SPECIES VARS ##

max_var_box_cox(zoo_spe, zoo_env.t, c("Conductivity", "pH", "Temperature", 'Depth'), NA, TRUE)

most_common_lambda =  sensitivity_analysis(zoo_spe, zoo_env.t, c("Conductivity", "pH", "Temperature", 'Depth'))

zoo_spe.trans = box_cox_trans(zoo_spe, most_common_lambda[1])


##### BUBBLE PLOTS ####

plot_bubble_map(zoo_community, "pH")
plot_bubble_map(zoo_community, "Conductivity")
plot_bubble_map(zoo_community, "Temperature")
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


