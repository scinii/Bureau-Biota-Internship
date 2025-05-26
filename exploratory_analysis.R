library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)
library(vegan)
library(ggplot2)
#library(sf)
library(ggOceanMaps)
library(ggspatial)
#library(ggrepel)
#library(patchwork)


setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') # set working directory

get_community_data <- function(df){
  
  # Description: 
  #              
  # Args:
  #       df: dataframe to be partitioned
  # Return: a list of two dataframe. The first one contains the counts for the
  #         the different group. The second one contains the value relative to
  #         the environmental variables.
    
  df$Taxa = NULL
  all_names = c("Location", unique(df$Name))
  df = pivot_wider(df, names_from = Name, values_from = Concentration, values_fill = 0) |>
      as.data.frame()
    
  # create non-environmental dataframe 
  non_env_df <- df[all_names]
  rownames(non_env_df) <- non_env_df$Location
  non_env_df$Location <- NULL
    
  # create environmental dataframe
  env_df <- df %>% select(-all_of(all_names))
  env_df <- env_df[!duplicated(env_df),]
  rownames(env_df) <- env_df$Location
  env_df$Location <- NULL
    
    
  return(list(df,non_env_df,env_df))
}


############ PLOTS ############

plot_bubble_map <- function(df, column_name){
  
  basemap(limits = c(11.5, 12.7, 78.85, 79),shapefiles = "Svalbard") + 
    theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
    geom_spatial_point(data = df, aes(x = lon, y = lat, size = .data[[column_name]]),color='red',shape = 1,stroke = 1.2) + 
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

which_year = "Year 2020"

data_yearly <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = which_year)

split_yearly_data <- get_community_data(data_yearly)

data_yearly_comb <- split_yearly_data[[1]]

data_yearly_conc <- split_yearly_data[[2]]

data_yearly_env <- split_yearly_data[[3]]


############ PLOTS ############

# BUBBLE SPECIES
plot_bubble_map(data_yearly_comb,"Macrothrix")
# BUBBLE ENV 
#plot_bubble_map(data_2024_comb,"pH")
# BUBBLE RICHNESS
#plot_bubble_map(data_2024_comb,"Richness")


############ CORRELATIONS ############






############ DCA ############
dca_yearly = decorana(data_yearly_conc, ira = 0) 









