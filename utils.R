library(openxlsx) # Open Excel files

library(dplyr) 
library(tidyr)
library(plyr)

library(vegan) # ordination functions

# plots
library(ggplot2)
library(corrplot)
library(naniar)

# spatial packages
library(sf)
library(ggOceanMaps)
library(ggspatial)
library(ggrepel)
library(sf)


get_community_data <- function(df, which_group){
  
  # Description: 
  #              
  # Args:
  #       df: dataframe to be partitioned
  # Return: a list of two dataframe. The first one contains the counts for the
  #         the different group. The second one contains the value relative to
  #         the environmental variables.
  
  var_to_keep = c('Location', 'pH', 'DO', 'Conductivity', 'Temperature',
                  'Depth', 'Drought', 'Counts', 'lat','lon','alt',which_group)
  
  df = df[ var_to_keep ] %>% drop_na(all_of(which_group))
  
  var_to_summ = var_to_keep[var_to_keep != 'Counts'] 
  df = ddply(df, var_to_summ, summarize, Counts = sum(Counts))
  
  all_names = unique(df[[which_group]])
  
  df = pivot_wider(df, names_from = which_group, values_from = Counts, values_fill = 0) |>
      as.data.frame()
  
  # create non-environmental dataframe 
  non_env_df = dplyr::select(df,all_of(all_names))
  rownames(non_env_df) <- df$Location
  
  # create environmental dataframe
  env_df <- dplyr::select(df, -all_of(all_names))
  #env_df <- env_df[!duplicated(env_df),]
  rownames(env_df) <- df$Location
  env_df$Location = NULL
  env_df$Drought = NULL
  env_df$alt = NULL
  env_df$Depth = NULL
  env_df$lat = NULL
  env_df$lon = NULL
    
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

############ PLOTS FUNCTIONS ############

lakes_map <- function(){
  
  lakes_location = read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")
  basemap(limits = c(11.5, 12.8, 78.8, 79),shapefiles = "Svalbard") + 
    theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
    geom_spatial_point(data = lakes_location, aes(x = lon, y = lat), color='red') + 
    geom_spatial_text_repel(data = lakes_location, aes(x = lon, y = lat, label = Location), max.overlaps = Inf) 
}
  
missing_data <- function(df, which_vars){
  
  if(which_vars == 'env'){
    df = df[c('pH','DO','Conductivity','Temperature','Depth')]
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
    geom_spatial_point(data = df, aes(x = lon, y = lat, fill=.data[[column_name]]),color="white",shape = 24, size = 5, stroke=0.5) +
    scale_fill_distiller(palette = "Reds", direction = 1) + 
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

plot_rda <- function(model, scaling_number){
  
  plot(model,
       scaling = scaling_number,
       display = c("sp", "lc", "cn"),
       main = paste("Informed RDA - Scaling", scaling_number)
       
  )
  
  if(scaling_number == 1){
    spe.sc1 <- scores(model, choices = 1:2, scaling = 1,display = "sp")
    arrows(0, 0,
           spe.sc1[, 1] * 0.9,
           spe.sc1[, 2] * 0.9,
           length = 0,
           lty = 1,
           col = "red")
      
  }
  else{
    spe.sc2 <- scores(model, scaling = 2,choices = 1:2,display = "sp")
    arrows(0, 0,
           spe.sc2[, 1] * 0.9,
           spe.sc2[, 2] * 0.9,
           length = 0,
           lty = 1,
           col = "red"
    )
  }
  
  
}
  
  









