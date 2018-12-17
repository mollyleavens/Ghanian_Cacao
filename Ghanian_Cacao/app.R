
library(readr)
library(shiny)
library(leaflet)
library(knitr)
library(kableExtra)
library(formattable)
library(tidyverse)

# Read in read_rds
both_waves <- read_rds("both_waves.rds")

# Define UI with navbar 
ui <- 
  navbarPage("Ghanian Cacao Farmers",
  
  tabPanel("About",
  
    fluidRow( 
     
  # Short description of survey  
   column(10, allign = "center",
          titlePanel("Ghanian Cacao Farmers"),
          hr(),
    p("This app summarizes some of the key variables for a survey of several thousand 
      cacao farmers across Ghana in both 2008 and 2014."),      
     h5(strong("Survey Overview")),
        p("Ghana is the world’s second largest cacao producing region after its neighbor 
            the Ivory Coast. The social, economic, and environmental conditions of these 
            farmers is therefore critical in assessing effective intervention methods and 
            predicting future global cacao market trends. 
        
            Harvard Professor Micheal Hiscox and Mondelez, the world’s largest chocolate company, jointly implemented 
            this survey. This app does not contain statistical analyses and is not meant to 
            claim correlations, causations, or significance. It is rather to display the 
            framework for the survey and some key aggregated data."),
     br(),    
    
    # This is a photo I took of a hand holding a cacao pod in Belize in October 2018  
        HTML('<center><img src="cacao_pod.jpg" height = 525 width = 350 ></center>'),  
   hr()))),
  
  # This Data tab then has 3 tabs within (Map, Univariate Distributions, and Summar Datatable)
  tabPanel("Data",
  fluidPage(
     column(12,
        tabsetPanel(type = "tabs",
              
             # Show map of survey distribution 
                    tabPanel("Map", 
                             h5("Distribution of survey locations"),
                             p("Each map point represents a single conducted survey"),
                             p("I understand there were not surveys conducted in the middle of the ocean.
                               However, I am working to find the source of this error before I filter them out."),
                             leafletOutput("surveymap")),
                    
             # This tab allows user to user to view univariate distributions
                    tabPanel("Univariate Distributions", 
                             br(),
                            # I will be adding more variable options soon once I have more discussions with my advisor about the data. 
                            # p("More variable options coming soon!"),
                             selectInput(inputId = "variable",
                                         label = "Choose a variable to view distribution:",
                                         choices = c("Household Size", "Number of Children",
                                                     "")),
                            plotOutput("boxPlot")),
            
             # Show summary table of        
                    tabPanel("Summary Datatable",  
                             h3("Percentage of respondents with access to key resources"),
                             hr(),
                             tableOutput("resources")
            
              ))))),
            # Authorship
            # Included personal college email for questions 
            # and a small photo of me on a cacao farm. 
            tabPanel("Authorship",
                     h4("Analysis and visualizations by Molly Leavens"),
                     p("Please direct any questions to mleavens at college.harvard.edu"),
                     HTML('<left><img src="molly_tree.jpg" height = 116.66 width = 175 ></left>')
  
))

# Define server logic 
server <- function(input, output) {
  
# Render map of survey locations 
   output$surveymap <- renderLeaflet ({
     leaflet() %>%
     addTiles() %>%  
     addCircles(lng=both_waves$l_gpslongitude, 
                lat=both_waves$l_gpslatitude, 
                weight = 1, radius = 1)
   })
   
# This allows the user to decide what variable gets graphed 
   variableInput <- reactive ({
     switch(input$variable,
            "Household Size" = both_waves$gghhsize, 
            "Number of Children" = both_waves$numChildren)
   })
   
# Render histogram of variable chosen by user    
   output$boxPlot <- renderPlot({
       ggplot(both_waves, aes_string(variableInput())) + geom_bar() + labs(x = input$variable)
   })
   
# Render percentage table under the "Summary Datatable" tab
  # Selecting for variables reduced to present/absent 1/0 yes/no
  # Each new line in the select() call corresponds to a variable grouping (ex - "Educational Resources")   
  # Named the new summary variables such that they would look polished on app
    output$resources <- function() {
     req(both_waves)
      both_waves %>% 
       select(surveyWave, ggliterate, ggeverschoolhhhead, 
              ggreceivedloan, ggFarmGroupOrCoop, ggbank,
              ggcar, ggchainsaw, ggmistblower, ggelectric,
              ggdumfert, gginsect, ggherb, ggfungi,
              ggTrainBusiness, ggTrainPlanting, ggTrainDivers, ggTrainEnviron, ggTrainMaint, ggTrainFert, ggTrainHealth, ggTrainChildLab
              ) %>% 
       group_by(surveyWave) %>% 
       summarise(
         # Educational Resources
                 `Literate` = 
                    percent(sum(ggliterate, na.rm = TRUE)
                    /(5371-sum(is.na(ggliterate))), digits = 0),
                 `Household head ever attended school` = 
                    percent(sum(ggeverschoolhhhead, na.rm = TRUE)
                    /(5371-sum(is.na(ggeverschoolhhhead))), digits = 0),
         # Financial Resources         
                 `Has ever received a loan` = 
                    percent(sum(ggreceivedloan, na.rm = TRUE)
                    /(5371-sum(is.na(ggreceivedloan))), digits = 0),
                 `Is member of farming organization or coop` = 
                    percent(sum(ggFarmGroupOrCoop, na.rm = TRUE)
                    /(5371-sum(is.na(ggFarmGroupOrCoop))), digits = 0),
                 `Has a bank account` = 
                     percent(sum(ggbank, na.rm = TRUE)
                     /(5371-sum(is.na(ggbank))), digits = 0),
         # Tools and Equipment
                 `Owns car` = 
                     percent(sum(ggcar, na.rm = TRUE)
                     /(5371-sum(is.na(ggcar))), digits = 0),
                 `Owns chainsaw` = 
                      percent(sum(ggchainsaw, na.rm = TRUE)
                      /(5371-sum(is.na(ggchainsaw))), digits = 0),
                 `Owns motorized mist blower` = 
                      percent(sum(ggmistblower, na.rm = TRUE)
                      /(5371-sum(is.na(ggmistblower))), digits = 0),
                 `Household has electricity` = 
                      percent(sum(ggelectric, na.rm = TRUE)
                      /(5371-sum(is.na(ggelectric))), digits = 0),
         # Chemical Inputs
                 `Uses fertilizer` = 
                      percent(sum(ggdumfert, na.rm = TRUE)
                      /(5371-sum(is.na(ggdumfert))), digits = 0),
                 `Uses insecticide` = 
                      percent(sum(gginsect, na.rm = TRUE)
                      /(5371-sum(is.na(gginsect))), digits = 0),
                 `Uses herbicide` = 
                      percent(sum(ggherb, na.rm = TRUE)
                      /(5371-sum(is.na(ggherb))), digits = 0),
                 `Uses fungicide` = 
                      percent(sum(ggfungi, na.rm = TRUE)
                      /(5371-sum(is.na(ggfungi))), digits = 0),
         # Extension Services
                 `Received accounting/business/entrepreneaurial training` = 
                      percent(sum(ggTrainBusiness, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainBusiness))), digits = 0),
                 `Received planting and farm expansion training` = 
                      percent(sum(ggTrainPlanting, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainPlanting))), digits = 0),
                 `Received crop diversification training` = 
                      percent(sum(ggTrainDivers, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainDivers))), digits = 0),
                 `Received deforestation and environment training` = 
                      percent(sum(ggTrainEnviron, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainEnviron))), digits = 0),
                 `Received farm maintenance training` = 
                      percent(sum(ggTrainMaint, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainMaint))), digits = 0),
                 `Received fertilizer/pesticide application training` = 
                      percent(sum(ggTrainFert, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainFert))), digits = 0),
                 `Received health and safety training` = 
                      percent(sum(ggTrainHealth, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainHealth))), digits = 0),
                 `Received child labor sensitization training` = 
                      percent(sum(ggTrainChildLab, na.rm = TRUE)
                      /(5371-sum(is.na(ggTrainChildLab))), digits = 0)) %>%
        
      # Restructure table so waves 1 and 2 are columns rather than rows
      # Add column with change between waves 1 and 2  
        gather(key, value, "Literate":"Received child labor sensitization training") %>% 
        spread("surveyWave", value) %>% 
        mutate(key = factor(key, levels = c("Literate", "Household head ever attended school",  
                                            "Has a bank account", "Has ever received a loan", "Is member of farming organization or coop",
                                            "Household has electricity", "Owns car", "Owns chainsaw", "Owns motorized mist blower", 
                                            "Uses fertilizer", "Uses fungicide", "Uses herbicide", "Uses insecticide",
                                            "Received accounting/business/entrepreneaurial training", 	
                                           "Received child labor sensitization training", 
                                          "Received crop diversification training",
                                         "Received deforestation and environment training",
                                        "Received farm maintenance training",
                                       "Received fertilizer/pesticide application training",
                                      "Received health and safety training",
                                     "Received planting and farm expansion training"))) %>% 
      
        arrange(key) %>% 
        mutate(change = (`1` - `2`)) %>% 
        kable("html", col.names = c("Variable", "Wave 1", "Wave 2", "Change")) %>% 
        kable_styling("striped", "hover") %>% 
      # Group simular variables together   
        group_rows("Educational Resources", 1, 2) %>%
        group_rows("Financial Resources", 3, 5) %>% 
        group_rows("Tools and Equipment", 6, 9) %>% 
        group_rows("Chemical Inputs", 10, 13) %>%
        group_rows("Extension Services ", 14, 21)
   }
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

