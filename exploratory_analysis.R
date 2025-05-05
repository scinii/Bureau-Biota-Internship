library(openxlsx) # Open Excel files
library(dplyr) 
library(tidyr)
library(vegan) # Perform CCA
library(sf) # Spatial Dataframes
library(ggplot2) 
library(corrplot) # Make Correlation Plots
library(usdm) # Package to calculate Variance Inflation Factor


# Set working directory
setwd('C:\\Users\\rober\\Documents\\GitHub\\Bureau-Biota-Internship') 


# Names of columns we want to study
variables_of_interest <- c("Location", "Year",	"pH",	"DO",	"Conductivity","Temperature",
                           "Depth",	"Drought",	"Name",	"Taxa","Concentration")
numeric_variables <- c("pH",	"DO",	"Conductivity",	"Temperature", "Depth",
                      "Drought", "Concentration")


# Load data
location_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "locations")
lakes_data <- read.xlsx(xlsxFile = "data_lakes.xlsx", sheet = "counts")[variables_of_interest]


# Data cleaning
lakes_data[numeric_variables] <- sapply(lakes_data[numeric_variables], as.numeric)
lakes_data <- na.omit(lakes_data)


# merge data and covert to spatial object
used_data <- merge(lakes_data,location_data,by="Location") |> st_as_sf(coords = c("lon", "lat"), crs = 3995)
            #st_transform(32633)


# Extract coordinates
coord <- as.data.frame(st_coordinates(used_data)) 
used_data$lon <- coord$X
used_data$lat <- coord$Y
used_data$geometry <- NULL


# Extract all year and lake codes
all_years_codes <- unique(used_data$Year)
all_lakes_codes <- unique(used_data$Location)


get_correlations = function(df,cols){
  
  # Description: creates a correlation plot and calculates 
  #              the Variance Inflation Factor (vif)
  #               
  # Args:
  #       df: dataframe of interest
  #       cols: columns that we want to include in our analysis
  #
  # Return: NA
  
  # Set colors and plot the correlation matrix
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  cPlot = corrplot(cor(df[cols]), method="color", col=col(200), type="upper",
          order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE)
  
  # Calculate vif
  vif(df[cols])
}

code_in_list <- function(list_codes, sample_code, which_code){
  
  # TODO: specify what the codes can be !!!! 
  # Description: This function checks if there is an element of list_codes 
  #              (a string) that is contained in the string sample_code
  # Args:
  #       list_codes: list of codes
  #       sample_code: reference string for checking
  #       which_code: "loc" if we are identifying the Lake.
  #                   "year" if we are identifying the Year
  # Return: TRUE if there is such element. FALSE otherwise
  
  in_list <- FALSE
  
  if(which_code == "loc"){
    
    for(i in 1:length(list_codes)){
      
      # check if the i-th element is contained in the sample_code string.
      # if yes then set in_list to TRUE and break as we only need one element
      # to be in the sample_code
      
      if( list_codes[i] == sample_code){
        in_list <- TRUE
        break
      }
    }
  }
  
  else if( which_code == "year"){
    
    for(i in 1:length(list_codes)){
      
      # same logic as the previous loop.
      
      if(grepl(list_codes[i],sample_code) == TRUE){
        in_list <- TRUE
        break
      }
    }
  }

  return(in_list)
  
}

get_data = function(lake_codes = all_lakes_codes,year_codes = all_years_codes){
  
  # Description: This function slice the dataframe according to the 
  #              lakes and years we want.
  # Args:
  #       lake_codes: list of codes that specify the lakes
  #       year_codes: list of codes that specify the years
  # Return: the dataframe we wanted
  
  # LOCATION SELECTION
  
  code_loc <- c()
  
  for(i in 1:length(samples_data$Location)){
    
    if( code_in_list(lake_codes, samples_data$Location[i], "loc") == TRUE){
      code_loc <- append(code_loc, i)
    }
  }
  
  data_code <- samples_data[code_loc,]
  
  # YEAR SELECTION
  
  year_loc <- c()
  
  for(i in 1:length(data_code$Year)){
    
    if( code_in_list(year_codes, data_code$Year[i], "year") == TRUE){
      year_loc <- append(year_loc, i)
    }
  }
  data_code <- data_code[year_loc,]
  
  return(data_code)
}

cca_data <- function(year_code){
  
  # Description: This function provides the two dataframes that are used in the 
  #              cca function of the vegan package
  # Args:
  #       year_code: which year you want to consider
  # Return: a list of two dataframe. The first one contains the counts for the
  #         the different Taxas. The second one contains the value relative to
  #         the environmental variables.
  
  year_data <- get_data(,year_code)
  
  
  taxa_df <- year_data|> 
            group_by(Location, pH, DO, Conductivity, Temperature, Depth, Drought, Taxa, lat, lon) |>
            summarise(
              Concentration = sum(Concentration),
              .groups = "drop"
            )  |> 
            pivot_wider(names_from = Taxa, values_from = Concentration, values_fill = 0) |> as.data.frame()
  
  cca_df <- taxa_df[c("Location", "Rotifera", "Cladocera","Copepoda")]
  rownames(cca_df) <- cca_df$Location
  cca_df$Location <- NULL

  env_df <- subset(taxa_df, select = -c(Rotifera, Cladocera,Copepoda))
  env_df <- env_df[!duplicated(env_df),]
  rownames(env_df) <- env_df$Location
  env_df$Location <- NULL

  return(list(cca_df,env_df))
}

cca_plot <- function(year_code, rhs_formula_string){
  
  # Description: This fgetunction performs cca and make the ordination plot
  #              
  # Args:
  #       year_code: which year you want to consider
  #       rhs_formula_string: the right hand side of the formula (i.e. which
  #       environmental variables you want to use)
  # Return: cca object of the vegan package.
  
  data <- cca_data(year_code)
  data_non_env <- data[[1]]
  data_env <- data[[2]]
  
  formula_cca <- as.formula(paste("data_non_env ~", rhs_formula_string))
  
  analysis <- cca(formula_cca, data = data_env)
  
  plot(analysis)
  return(analysis)
}




#yr = "2024"
#con_ph = cca_plot(yr,"Conductivity + pH")
#ph_coord = cca_plot(yr,"pH + lon + lat")
#con_coord = cca_plot(yr,"Conductivity + lon + lat")
#con_ph_coord = cca_plot(yr,"Conductivity + pH  + lon + lat")

