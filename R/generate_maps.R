##-------------------------------------------------------##
##                      GENERATE MAPS                    ##
##-------------------------------------------------------##

#----------------#
# Prepare Script #
#----------------#

library(plotly)
library(glue)
library(magrittr)
library(tidyverse)

# Set working directory. 
setwd(dir = "/Users/juanitorduz/Documents/welthungerhilfe_index_viz")

# Set input file name. 
input_file_name <- "GHI2018_data.xlsx"

# Get variable names (= excel sheet names). 
var_names <- readxl::excel_sheets(path = glue("Data/{input_file_name}"))

#-------------------#
# Support Functions #
#-------------------#

#' Constructs variable data frame containing the variable data 
#' for all the years plus a country code for all countries in the world. 
#'
#' @param var_name Variable name. 
#' @return Variable data frame. 
get_variable_data_frame <- function(var_name) {
 
  # Read data.
  raw_df <- readxl::read_excel(path = glue("Data/{input_file_name}"), sheet = var_name) %>% 
            mutate_all(.funs = as.character)

  # Round numbers to 2 digits (when possible).
  data_df <- raw_df %>% 
              mutate_all(.funs = ~ if_else(condition = (.x %>% as.numeric %>% is.na), 
                                           true = .x, 
                                           false = .x %>% as.numeric %>% round(digits = 2) %>% as.character))
  
  # Read in country codes for countries in the raw data. 
  country_codes <- readxl::read_excel(path = "Data/country_codes.xlsx")
  # Add country codes for these countries. 
  data_df <- left_join(x = data_df, y = country_codes, by = 'Country') 
  # Read country codes for all countries in the world (we need them all for the plot). 
  raw_country_codes <- readxl::read_excel(path = "Data/country_codes_raw.xlsx")
  # Calculate the countries which are not in the raw data. 
  additional_countries <- anti_join(x = raw_country_codes, 
                                    y = data_df %>% select(Country, Code), 
                                    by = "Code")
  # Add these missing countries to have a complete coverage of the world.
  data_df %<>% bind_rows(additional_countries) %>% arrange(Country)
  
  return(data_df)
  
}


#' From a data vector, define the corresponding element-wise categories.
#'
#' @param y Data vector. 
#' @return Category vector.
encode_rank <- function(y){
 
  y %>% map_chr(.f = function(x) {
    
    if (is.na(x)) { return('No Data') }
    
    n <- x %>% as.numeric 
    
    if (is.na(n)) {
      
      if (x == '<5') {return('Insufficient Data')} 
      
      else { 'No Data (Risky)' }
      
    } else {
      
      if (n >= 50) { return('Extremely Alarming') }
      else if (n >= 35) { return('Alarming') }
      else if (n >= 20) { return('Serious') }
      else if (n >= 10) { return('Moderate') } 
      else return('Low')
      
    }
  })
}


#' Constructs variable data frame for a given year.
#'
#' @param var_name_sym Variable (symbol) name. 
#' @param data_df Output of the GetVariableDataFrame function.
#' @param year Year to be considered.
#' @return Variable-year dat frame. 
get_variable_year_df <- function(var_name_sym, data_df, year) {

  levels_vector <- c('Extremely Alarming', 
                      'Alarming', 'Serious', 
                      'Moderate', 'Low', 
                      'Insufficient Data', 
                      'No Data (Risky)', 
                      'No Data')
  
  year_df <- data_df %>% 
              select(Country, year, Code) %>% 
              rename(!!var_name_sym := year) %>% 
              mutate(Rank = encode_rank(!!var_name_sym)) %>% 
              mutate(Rank = Rank %>% as.factor) %>% 
              mutate(Rank = Rank %>% fct_relevel(... = levels_vector))
  
  return(year_df)
}


#' Constructs the data frame to generate the map.
#'
#' @param var_name_sym Variable (symbol) name.
#' @param year_df Output of the GetVariableYearDataFrame function.
#' @return Data frame containing polygon data (lat-lon) to draw the maps.
construct_map_df <- function(var_name_sym, year_df) {

  library(maps)
  
  # Get world map data.
  map_data_df <- map_data(map = 'world')
  # Generate ISO Code and merge with year.df.
  map_data_df %<>% mutate(Code = iso.alpha(x = region, n = 3)) %>% 
                   left_join(y = year_df, by = 'Code')
  # Define country Info for visualization.
  map_data_df %<>% mutate(Info = if_else(condition = is.na(!!var_name_sym), 
                                         true = str_c('\n Country : ', Country), 
                                         false = str_c('\n Country : ', Country, ' \n', 
                                                       as.character(var_name_sym), ': ', !!var_name_sym)))
  # Remove maps package. 
  detach('package:maps', unload=TRUE)
  
  return(map_data_df)
}

# Define color for each rank level.
color_scale <- c('No Data' = '#ffffff', 
                 'Insufficient Data' = '#c6c6c6', 
                 'No Data (Risky)' = '#A0522D', 
                 'Extremely Alarming' = '#990033', 
                 'Alarming' = '#ff9900', 
                 'Serious' = '#ffcc00', 
                 'Moderate' = '#99cc99', 
                 'Low' = '#339933')


#' Generate map.
#'
#' @param map_data_df Output of the construct_map_df function.
#' @param var_name Variable name. 
#' @param year Year to be considered.
#' @param color_scale Color scale for the different categories. 
#' @param save_map Boolean option to save mas as .html.
#' @return World map for the correspinding variable-year.
generate_map <- function(map_data_df, var_name, year, color_scale, save_map = FALSE) {

  plt <- map_data_df %>% 
          # Remove counntries with no information at all. 
          filter(! is.na(Info)) %>% 
          # Definne plot object. 
          ggplot(mapping = aes(x = long, y = lat, group = group)) + 
          # Customize theme. 
          theme(
            
            panel.background = element_rect(fill = 'lightblue',
                                            colour = 'lightblue',
                                            size = 0.5,
                                            linetype = 'solid'),
            
            panel.grid.major = element_line(size = 0.5, 
                                            linetype = 'solid',
                                            colour = 'lightblue'), 
            
            panel.grid.minor = element_line(size = 0.25, 
                                            linetype = 'solid',
                                            colour = 'lightblue'), 
            
            panel.border = element_rect(colour = 'black', fill = NA, size = 2),
            
            # Set color as in Shiny App background. 
            plot.background = element_rect(fill = '#ECF0F5')
            
          ) +
          # Draw map: Color by rank. 
          geom_polygon(mapping = aes(fill = Rank, label = Info), 
                       color = 'black',
                       size = 0.1) +
          # Set color scale.
          scale_fill_manual(values = color_scale) + 
          # Add tittle and axis labels. 
          ggtitle(label = glue('{var_name} : {year}')) + xlab(label = '') + ylab(label = '') 
        
  plt_ly <- plt #%>% ggplotly
  
  if (save_map) { htmlwidgets::saveWidget(widget = plt_ly, file = 'map.html') }
  
  
  return(plt_ly)
}


generate_map_data_var_year <- function(var_name, year) {
  
  var_name_sym <- rlang::sym(var_name)
  
  data_df <- get_variable_data_frame(var_name = var_name)
  
  year_df <- get_variable_year_df(var_name_sym = var_name_sym, data_df = data_df, year = year)
  
  map_data_df <- construct_map_df(var_name_sym = var_name_sym, year_df = year_df)
  
  map_data_df %<>% mutate(Year = year, Variable = var_name)
  
  return(map_data_df)
}


# all_map_data_list <- vector(mode = "list")
# 
# for (var_name in var_names) {
#   
#   for (year in years) {
#     
#     all_map_data_list[[var_name]][[year]] <- generate_map_data_var_year(var_name = var_name, year = year)
#     
#   }
# }

#---------------#
# Main Function #
#---------------#

#' Generate map given a variable name and a year. 
#'
#' @param var_name Variable name. 
#' @param year Year to be considered.
#' @param color_scale Color scale for the different categories. 
#' @param save_map Boolean option to save mas as .html.
#' @return World map for the correspinding variable-year.
generate_map_var_year <- function(var_name, year, color_scale, save_map = FALSE) {
  
  var_name_sym <- rlang::sym(var_name)
  
  data_df <- get_variable_data_frame(var_name = var_name)
  
  year_df <- get_variable_year_df(var_name_sym = var_name_sym, data_df = data_df, year = year)
  
  map_data_df <- construct_map_df(var_name_sym = var_name_sym, year_df = year_df)
  
  plt_ly <- generate_map(map_data_df = map_data_df,
                         var_name = var_name, 
                         year = year, 
                         color_scale = color_scale, 
                         save_map = save_map)
  
  return(plt_ly)
}


generate_map_var_year2 <- function(all_map_data, var_name, year, color_scale, save_map = FALSE) {
  
  map_data_df <- all_map_data %>% filter(Variable == var_name, Year == year)
  
  plt_ly <- generate_map(map_data_df = map_data_df,
                         var_name = var_name, 
                         year = year, 
                         color_scale = color_scale, 
                         save_map = save_map)
  
  return(plt_ly)
}

#----------------------------#
# Generate RDS with All Data #
#----------------------------#

# all_data <- var_names %>% map_df(.f = ~ get_variable_data_frame(var_name = .x) %>% 
#                                         add_column(Variable = .x, .before = "Country"))
# 
# saveRDS(object = all_data, file = "Data/all_data.rds")

#---------#
# Example #
#---------#


# generate_map_var_year(var_name = "Index",
#                       year = "2018", 
#                       color_scale = color_scale,
#                       save_map = FALSE)
