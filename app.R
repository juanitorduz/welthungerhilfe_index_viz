## app.R ##

library(feather)
library(glue)
library(magrittr)
library(plotly)
library(tidyverse)

library(DT)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

source(file = "generate_maps.R")

#-----------#
# Load Data #
#-----------#

all_data <- readRDS(file = "Data/all_data.rds")

var_names <- all_data %>% pull(Variable) %>% unique

years <- colnames(all_data) %>% setdiff(y = c("Variable", "Country", "Code"))

all_map_data  <- read_feather(path = "Data/all_map_data.feather")


#-----------#
# Shiny App #
#-----------#

ui <- dashboardPage(
  
  skin = 'black',
  
  dashboardHeader(title = 'WELTHUNGER-INDEX'),
  
  dashboardSidebar(
    
    sidebarMenu(
    
      menuItem(text = 'World Map', tabName = 'world_map', icon = icon(name = 'globe')),
      
      menuItem(text = 'Statistics', tabName = 'statistics', icon = icon(name = 'industry')), 
      
      menuItem(text = 'Data', tabName = 'Data', icon = icon(name = 'database'))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = 'world_map',
              
              h1('Wold Map'), 
              
              fluidPage(title = 'World_Map',
                
                plotlyOutput(outputId = 'map', width = 'auto', height = 700) %>% withSpinner(type = 4), 
      
                selectInput(inputId = 'select_variable_map', 
                            label = 'Select Variable', 
                            choices = var_names, 
                            selected = var_names %>% head(1)), 
                
                selectInput(inputId = 'select_year_map', 
                            label = 'Select Year', 
                            choices = years, 
                            selected = years %>% tail(1)), 
                
                h2('Description'),
                
                h3(textOutput(outputId = 'map_text_description'))
                
        )
      ),
      
      tabItem(tabName = 'Data',
              
              h1('WELTHUNGER-INDEX Data'), 
              
              fluidPage(title = h2('Data'),
                        
                        box(width = 12,
                        
                          dataTableOutput(outputId = 'data')
                          
          ),
                        
                        downloadButton(outputId = 'download_data', label = 'Download')
                        
        )
      )
    )
  )
)


server <- function(input, output) { 
  
  output$map <- renderPlotly({
    
    generate_map_var_year2(all_map_data = all_map_data,
                           var_name = input$select_variable_map, 
                           year = input$select_year_map, 
                           color_scale = color_scale, 
                           save_map = FALSE)

  })
  
  output$map_text_description <- renderText({
    
    'map descpription... '
    
    })
  
  output$data <- renderDataTable({
    
    all_data 
    
  })
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      'welthunger_index.csv'
    },
    
    content = function(file) {
      write_csv(x = all_data, path = file, na = '')
    }
  )

}

shinyApp(ui, server)
