library(openxlsx) # Open Excel files

library(dplyr) 
library(tidyr)
library(plyr)

library(DescTools) # statistical package
library(vegan) # ordination functions
library(memisc) # missing data
# source("cleanplot.pca.R") # plots PCA

# plots
library(ggplot2)
library(corrplot)
library(naniar)

# spatial packages

library(ggOceanMaps)
library(ggspatial)
library(ggrepel)

split_rotifers_arthropodas <- function(df, which_group){
  
  "
  :param df: dataframe that contains the vars (see below) columns. 
  :param which_group: which group of the arthropodas to keep.  
  :return: Three dataframes where each row corresponds to a lake. 
           - non_env_dataframe has as columns the different groups of arthropodas
                               and one for rotifers (the counts have been aggregated)
           - env_dataframe has as columns the environmental variables pH, DO, Conductivity and Temperature
           - df's columns are the ones of the two dataframes just mentioned plus Depth, Drought, lat, lon and Altitude
  "
  
  vars = c('Location', 'pH', 'DO', 'Conductivity', 'Temperature','Depth', 'Drought', 'Counts', 'lat','lon','Altitude')
  
  
  # select rotifers and aggregate the counts
  vars_rotifers = c(vars, 'Phylum')
  rotifers = df[df$Phylum == "Rotifera",][vars_rotifers]
  rotifers = ddply(rotifers, vars_rotifers[vars_rotifers != 'Counts'] , summarize, Counts = sum(Counts))
  rotifers = rename(rotifers, Phylum = Taxa)
  
  # select arthropodas and aggregate the counts of common groups
  vars_arthropodas = c(vars, which_group)
  arthropodas = df[!df$Phylum == "Rotifera",][vars_arthropodas] %>% drop_na(all_of(which_group))
  arthropodas = ddply(arthropodas, vars_arthropodas[vars_arthropodas != 'Counts'], summarize, Counts = sum(Counts))
  arthropodas = rename(arthropodas, Genus = Taxa)
  
  df = rbind(rotifers, arthropodas)
  
  
  all_names = unique(df$Taxa)
  
  df = pivot_wider(df, names_from = Taxa, values_from = Counts, values_fill = 0) |>
    as.data.frame()
  
  
  # create non environmental dataframe
  non_env_df = dplyr::select(df,all_of(all_names))
  rownames(non_env_df) <- df$Location
  
  # create environmental dataframe
  env_df <- dplyr::select(df, -all_of(all_names))
  rownames(env_df) <- df$Location
  env_df$Location = NULL
  env_df$Drought = NULL
  env_df$DO = NULL
  env_df$Altitude = NULL
  env_df$lat = NULL
  env_df$lon = NULL
  
  return(list(df,non_env_df,env_df))
  
}

get_community_data <- function(df, which_group){
  
  
  var_to_keep = c('Location', 'pH', 'DO', 'Conductivity', 'Temperature',
                  'Depth', 'Drought', 'Counts', 'lat','lon','Altitude',which_group)
  
  df = df[ var_to_keep ] %>% drop_na(all_of(which_group))
  
  var_to_summ = var_to_keep[var_to_keep != 'Counts'] 
  df = ddply(df, var_to_summ, summarize, Counts = sum(Counts))
  
  all_names = unique(df[[which_group]])
  
  df = pivot_wider(df, names_from = all_of(which_group), values_from = Counts, values_fill = 0) |>
      as.data.frame()
  
  # create non-environmental dataframe 
  non_env_df <- dplyr::select(df,all_of(all_names))
  rownames(non_env_df) <- df$Location
  
  # create environmental dataframe
  env_df <- dplyr::select(df, -all_of(all_names))
  #env_df <- env_df[!duplicated(env_df),]
  rownames(env_df) <- df$Location
  env_df$Location = NULL
  env_df$Drought = NULL
  env_df$DO = NULL
  env_df$Altitude = NULL
  env_df$lat = NULL
  env_df$lon = NULL
    
  return(list(df,non_env_df,env_df))

}

diversity_table <- function(df,vars){
  
  
  data_conc = df[vars]
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
  
  " Returns a map of the Kongsfjorden area and the location of the lakes "
  
  lakes_location = read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")
  basemap(limits = c(11.5, 12.8, 78.8, 79),shapefiles = "Svalbard") + 
    theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
    geom_spatial_point(data = lakes_location, aes(x = lon, y = lat), color='red') + 
    geom_spatial_text_repel(data = lakes_location, aes(x = lon, y = lat, label = Location), max.overlaps = Inf)+
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
    )  
}
  
missing_data <- function(df, which_vars){
  
  "
  :param df: dataframe that contains the 'env' or 'taxa' variables below
  :param which_vars: env if we want the environmental variables. Otherwise taxas are considered.
  :return: a plot with histograms of the missing value for easy visualization.
  
  "
  
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
  
  "
  :param df: dataframe that contains longitude, latitude and the column_name
  :param column_name: oclumn whose values we want to plot
  :return: a plot of the Kongsfjorden area and each lake is represented by a triangle
           (to indentify its location) and a colour associated with the values in the column
  "

  basemap(limits = c(11.5, 12.7, 78.85, 79),shapefiles = "Svalbard") + 
    theme(panel.background = element_rect(fill = "lightblue"),panel.ontop = FALSE) +
    geom_spatial_point(data = df, aes(x = lon, y = lat, fill=.data[[column_name]]),color="white",shape = 24, size = 5, stroke=0.5) +
    scale_fill_distiller(palette = "Reds", direction = 1) + 
    labs(fill = paste(column_name)) + 
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
    ) 
}


plot_ordination <- function(model, which_ordination, scaling){
  
  "
  :param model: the pca or rda model
  :param which_ordination: decide between pca or rda plots
  :scaling: type 1 or 2
  :return: the ordination plot with the given scaling
  "
  
  if(which_ordination == "pca"){
    
    biplot(model, scaling = scaling)
    
    if(scaling == 1){
      pcacircle(model, 1, 1)
    }
    
  }
  else{
    
    if(scaling == 1){
      
      plot(model,scaling = 1, display = c("sp", "lc", "cn"))
      spe.sc1 <- scores(model, scaling =1,choices = 1:2,display = "sp")
      arrows(0, 0,spe.sc1[, 1] * 0.9 ,spe.sc1[, 2] * 0.9 ,length = 0,lty = 1,col = "red")

    }
    else{
      
      plot(model,scaling = 2, display = c("sp", "lc", "cn"))
      spe.sc2 <- scores(model, scaling = 2,choices = 1:2,display = "sp")
      arrows(0, 0,spe.sc2[, 1] * 0.9 ,spe.sc2[, 2] * 0.9 ,length = 0,lty = 1,col = "red")
      
    } 
    
    
  }
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
  
##### TRANSFORMATIONS ######


box_cox_trans <- function(raw_matrix, lambda){
  
  "
  :param raw_matrix: matrix of species abundance data
  :param lambda: exponent of the box-cox transformation
  :return: the transformed dataframe
  "
  
  if(lambda == 0){
    transformed_data = log1p(raw_matrix)
  }
  else{
    transformed_data = raw_matrix ** lambda
  }
  
  transformed_data = decostand(transformed_data, "normalize")
  
  return (transformed_data)
  
}


max_var_box_cox <- function(raw_matrix, expl_matrix, variables, w_var, plot_bool){
  
  "
  :param raw_matrix: matrix of untransformed species abundance data
  :param expl_matrix: matrix of explanatory variables
  :param variables: list of variables to consider in the RDA model (must be in expl_matrix)
  :param w_var: weight of the optimization problem between explained variance and fraction of variance RDA can maximally explain
  :param plot_bool: if TRUE then the two sub-objective functions are plotted
  :return: the lambda that maximizes the objective function.
  "
  
  lambdas = seq(0,1,0.05)
  
  variances = vector( "numeric" , length(lambdas) )
  max_variances = vector( "numeric" , length(lambdas) )
  
  for(i in 1:length(lambdas)){
    
    transformed_data = box_cox_trans(raw_matrix, lambdas[i])
    
    
    formula <- as.formula(paste("transformed_data", paste(variables, collapse = " + "), sep = " ~ "))
    
    rda_model = rda(formula, expl_matrix)
    
    pca_model = rda(transformed_data)
    
    
    variances[i] = RsquareAdj(rda_model)$r.squared
    
    max_variances[i] = variances[i]/( sum(pca_model$CA$eig[1:3]) / sum(pca_model$CA$eig[1:length(pca_model$CA$eig)]) )
    
  }
  
  
  if(plot_bool == TRUE){
    
    plot(lambdas,variances, ylab = "Explained Variance", xlab = "Lambda", type = "p", bg="red", pch = 21, col = "red", ylim = c(min(variances), max(max_variances)) )
    points(lambdas, max_variances, type = "p", bg="blue", pch = 21, col = "blue")
    legend(x="bottomright", legend = c("Variance explained by RDA","Maximum Variance RDA could explain"), fill= c("red","blue"), bg="transparent")
    
  }
  
  w_max_var = 1 - w_var 
  
  variance_tradeoff = w_var*variances + w_max_var*max_variances
  
  
  return( lambdas[which.max(variance_tradeoff)] )
}


sensitivity_analysis <- function(raw_matrix, expl_matrix, variables){
  
  "
  :param raw_matrix: matrix of untransformed species abundance data
  :param expl_matrix: matrix of explanatory variables
  :param variables: list of variables to consider in the RDA model (must be in expl_matrix)
  :return: the lambda that maximizes the objective function most often (i.e. mode) for different weight of the subobjectives
  "
  
  
  w_var = seq(0,1,0.05)
  best_lambdas = vector( "numeric" , length(w_var) )
  
  for(i in 1:length(w_var)){
    
    best_lambdas[i] = max_var_box_cox(raw_matrix, expl_matrix, variables, w_var[i], FALSE)
    
  }
  
  plot(w_var,best_lambdas, ylab = "Value of best lambda", xlab = "Weigth for variance", type = "p", bg="red", pch = 21, col = "red")
  
  return(Mode(best_lambdas))
}
  
  




