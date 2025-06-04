library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)
library(plyr)
library(vegan) 
library(ggplot2)
library(ggOceanMaps) # Svalbard's Map
library(ggspatial) # Svalbard's Map
library(corrplot)
library(naniar) # visualize missing data

setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory


############ CONTINGECY TABLES FUNCTION ############

#Species, Genus, Family, Order, Class, Phylum

get_community_data <- function(df, which_group){
  
  # Description: 
  #              
  # Args:
  #       df: dataframe to be partitioned
  # Return: a list of two dataframe. The first one contains the counts for the
  #         the different group. The second one contains the value relative to
  #         the environmental variables.
  
  var_to_keep = c('Location', 'pH', 'DO', 'Conductivity', 'Temperature',
                  'Depth', 'Drought', 'Counts', 'lat','lon',which_group)
  
  df = df[ var_to_keep ] %>% drop_na(all_of(which_group))
  var_to_summ = var_to_keep[var_to_keep != 'Counts'] 
  df = ddply(df, var_to_summ, summarize, Counts = mean(Counts))
  
  all_names = c("Location") %>% append(unique(df[[which_group]]))
  
  
  df = pivot_wider(df, names_from = which_group, values_from = Counts, values_fill = 0) |>
      as.data.frame()

  # create non-environmental dataframe 
  non_env_df <- df %>% select(all_of(all_names))
  rownames(non_env_df) <- non_env_df$Location
  non_env_df$Location <- NULL

  
  # create environmental dataframe
  env_df <- df %>% select(-all_of(all_names))
  env_df <- env_df[!duplicated(env_df),]
  rownames(env_df) <- env_df$Location
  env_df$Location <- NULL
    
  return(list(df,non_env_df,env_df))
}

diversity_table <- function(df){
  
  data_conc = df[,4:ncol(df)]
  N0 <- rowSums(data_conc>0)
  N1 <- exp( diversity(data_conc, index = "shannon") )
  N2 <- diversity(data_conc, index="invsimpson")
  E1 <- N1 / N0
  E2 <- N2 / N0
  diversity <- data.frame(N0,N1,N2,E1,E2)
  diversity$lat = df$lat
  diversity$lon = df$lon
  diversity$Location = df$Location
  return(diversity)
  
}

cca_plot <- function(data_non_env, data_env, rhs_formula_string){

  formula_cca <- as.formula(paste("data_non_env ~", rhs_formula_string))
  
  analysis <- cca(formula_cca, data = data_env)
  
  plot(analysis)
  return(analysis)
}


############ PLOTS FUNCTIONS ############

missing_data <- function(df, which_vars){
  
  if(which_vars == 'env'){
    df = df[c('pH','DO','Conductivity','Temperature','Depth','Drought')]
  }
  else{
    df = df[c('Species','Genus', 'Family', 'Order', 'Class', 'Phylum')]
  }
  
  if(n_var_miss(df) > 0){
    missing2 = vis_miss(df) +
    theme(plot.title = element_text(hjust = 0.5))
    print(missing2)
  }
}

plot_bubble_map <- function(df, column_name){
  
  basemap(limits = c(11.5, 12.7, 78.85, 79),shapefiles = "Svalbard") + 
    theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
    geom_spatial_point(data = df, aes(x = lon, y = lat, size = .data[[column_name]]),color='white', shape = 21, fill='red', stroke=0.5) + 
    geom_spatial_text_repel(data = df, aes(x = lon, y = lat, label = Location), max.overlaps = Inf) + 
    scale_size(range = c(2, 10)) + 
    scale_x_continuous(
      name = "Longitude",
      breaks = seq(11.5, 12.7, by = 0.3),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
    name = "Latitude",
    breaks = seq(78.85, 79.0, by = 0.05), 
    expand = c(0, 0)
    ) + 
    ggtitle( paste(column_name)  ) + 
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) 
}

plot_frequency <- function(df){
  
  hist(apply(df > 0, 2, sum),
       main = "Species Occurrences",
       right = FALSE,
       las = 1,
       xlab = "Number of occurrences",
       ylab = "Number of species",
       # length(unique(df$Location)
       breaks = seq(0, nrow(df), by = 1),
       col = "bisque"
  )
}


############ GET YEARLY DATA ############

which_year = "Year 2024"

data_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

split_yearly_data <- get_community_data(data_yearly, 'Genus')

data_yearly_comb <- split_yearly_data[[1]]

data_yearly_conc <- split_yearly_data[[2]]

data_yearly_env <- split_yearly_data[[3]]


# Diversities

div_2024 =  diversity_table(data_yearly_comb[,c(1,8:27)])

############ PLOTS ############


## MISSING DATA
#missing_data(data_yearly,'env')
#missing_data(data_yearly, 'groups')


# BUBBLE SPECIES
#plot_bubble_map(data_yearly_comb,"Macrothrix")
# BUBBLE ENV 
#plot_bubble_map(data_2024_comb,"pH")
# BUBBLE RICHNESS
#plot_bubble_map(data_2024_comb,"Richness")


############ CORRELATIONS ############

bray_curtis_diss = vegdist(log1p(data_yearly_conc), method = "bray")

corrplot(as.matrix(bray_curtis_diss), is.corr = FALSE, method = 'color',
         col = COL1('Oranges'), cl.pos = 'r', addgrid.col = 'white', addCoef.col = 'black', type= "lower", diag = FALSE)



############ DCA ############

dca_yearly = decorana(data_yearly_conc, ira = 0) 









