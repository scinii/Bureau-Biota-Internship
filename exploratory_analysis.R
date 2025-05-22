library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)
library(vegan)
library(ggplot2)
library(sf)
library(ggOceanMaps)
library(ggspatial)
library(ggrepel)
library(patchwork)


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

plot_bubble_map <- function(df, column_names){
  
  plot_list <- vector("list", length(column_names)) 
  
  for(i in 1:4){
    plot_list[[i]] = basemap(limits = c(11.5, 12.7, 78.85, 79),shapefiles = "Svalbard") + 
      theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
      geom_spatial_point(data = df, aes(x = lon, y = lat, size = .data[[column_names[i]]]),color='red',shape = 1,stroke = 1.2) + 
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
      ggtitle( paste(column_names[i])  ) + 
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  }
  combined = ( plot_list[[1]] | plot_list[[2]]) / (plot_list[[3]] | plot_list[[4]])
  return(combined)
}

# BUBBLE SPECIES
#plot_bubble_map(data_2024_comb,c("Macrothrix","Lecane","Chydorus","Rotifera"))
# BUBBLE ENV 
#plot_bubble_map(data_2024_comb,c("pH","DO","Conductivity","Temperature"))


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

###############################


############ GET YEARLY DATA ############

data_2024 <- read.xlsx(xlsxFile = "yearly_data.xlsx", sheet = "Year 2024")

split_2024_data <- get_community_data(data_2024)

data_2024_comb <- split_2024_data[[1]]

data_2024_conc <- split_2024_data[[2]]

data_2024_env <- split_2024_data[[3]]

#########################################







