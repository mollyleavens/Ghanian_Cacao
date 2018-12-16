
library(readr)
library(shiny)
library(leaflet)
library(knitr)
library(kableExtra)
library(tidyverse)

# Read in read_rds
both_waves <- read.csv("farmerData_bothWaves_copy.csv")
chem_resources <- read_rds("chem_resources.rds")
fin_resources <- read_rds("fin_resources.rds")
phys_resources <- read_rds("phys_resources.rds")
ed_resources <- read_rds("ed_resources.rds")

# Define UI 
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
        
                              Harvard Professor Micheal 
                               Hiscox and Mondelez, the world’s largest chocolate company, jointly implemented 
                               this survey. This app does not contain statistical analyses and is not meant to 
                               claim correlations, causations, or significance. It is rather to display the 
                               framework for the survey and some key aggregated data."),
    
        HTML('<center><img src="cacao_pod.jpg" height = 525 width = 350 ></center>'),  
   hr()))),
  
  tabPanel("Data",
  fluidPage(
   
      
        #add option where you can color by gender
        #add option to separate by region ()
        #provide instructions h5("Click on varible to see the distribution of this variable"),
         #actionButton("gghhsize", "House Hold Size"),
         #actionButton("ggfraclost", "Fraction of Harvest Lost to Problems"),
         #tabs for wave 1 and 2 or both
         #conditionalPanel(condition = "tabPanel == `Box Plots`",
      
      column(12,
        tabsetPanel(type = "tabs",
                    
                    
                  #  htmlOutput("cacao_photo")),
             # Show map of survey distribution 
                    tabPanel("Map", 
                             h5("Distribution of survey locations"),
                             p("Each map point represents a single conducted survey"),
                             leafletOutput("surveymap")),
                    
             # This tab allows user to user to view univariate distributions
                    tabPanel("Univariate Distributions", 
                             br(),
                             p("More variable options coming soon!"),
                             selectInput(inputId = "variable",
                                         label = "Choose a variable to view distribution:",
                                         choices = c("House Hold Size", "Number of Children",
                                                     "")),
                            plotOutput("boxPlot")),
             
            # This tab allows user to user to view relationship between two variables
            tabPanel("Multivariate Distributions",
                     br(),
                     p("Coming soon!")),
             #         selectInput(inputId = "variable",
              #                    label = "Choose a variable:",
               #                   choices = c("House Hold Size", "Number of Children", "Age of House Hold Head",
                #                              "")),
                 #     plotOutput("boxPlot")),
             
             # Show knitr tables       
                    tabPanel("Summary Datatables",  
                             h3("Percentage of respondents with access to key resources"),
                             
                             h5("Educational Resources"),
                             tableOutput("ed_resources"),
                             
                             h5("Financial Resources"),
                             tableOutput("fin_resources"),
                             
                             h5("Chemical Resources"),
                             tableOutput("chem_resources"),
                             
                             h5("Physical Resources"),
                             tableOutput("phys_resources") 
                                  
            
              ))))),
            # Authorship
            tabPanel("Authorship",
                     h4("Analysis and visualizations by Molly Leavens"),
                     p("Please direct any questions to mleavens at college.harvard.edu"),
                     HTML('<left><img src="molly_tree.jpg" height = 116.66 width = 175 ></left>')
  
        

))


# Define server logic 
server <- function(input, output) {
  
# Render map of survey locations 
# Removed   
   output$surveymap <- renderLeaflet ({
     leaflet() %>%
     addTiles() %>%  
     addCircles(lng=both_waves$l_gpslongitude, 
                lat=both_waves$l_gpslatitude, 
                weight = 1, radius = 1)
   })
   
# This allows the user to decide what variable gets graphed 
   variableInput <- reactive({
     switch(input$variable,
            "House Hold Size" = both_waves$gghhsize, 
            "Number of Children" = both_waves$numChildren, 
            "Age of House Hold Head" = both_waves$ggmeanage)
   })
   
# Render histogram of variable chosen by user    
   output$boxPlot <- renderPlot({
       ggplot(both_waves, aes_string(variableInput())) + geom_bar() + labs(x = input$variable)
     })
   
# Render percentage table under the "Summary Datatable" tab
   
  # Creating educational resource data table 
  # Selecting for variables reduced to present/absent 1/0 yes/no
  # Each new line in the select() call corresponds to a variable grouping (ex - education, training)   
  # Named the new summary variables such that they would look polished on app
    output$ed_resources <- function() {
     both_waves %>% 
       select(surveyWave, ggtraining, ggliterate, ggeverschoolhhhead, 
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
                 `House hold head ever attended school` = 
                    percent(sum(ggeverschoolhhhead, na.rm = TRUE)
                    /(5371-sum(is.na(ggeverschoolhhhead))), digits = 0),
         # Financial Resources         
                 `Has ever received loan` = 
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
      # spread(surveyWave, "value")
      #  "surveyWav "Literate"	"House hold head ever attended school""	Has ever received loan	Is member of farming organization or coop	Has a bank account	Owns car	Owns chainsaw	Owns motorized mist blower	Household has electricity	Uses fertilizer	Uses insecticide	Uses herbicide	Uses fungicide	Received accounting/business/entrepreneaurial training	Received planting and farm expansion training	Received crop diversification training	Received deforestation and environment training	Received farm maintenance training	Received fertilizer/pesticide application training	Received health and safety training	Received child labor sensitization training
        
      #  %>% 
       gather(key = "Variable", value = -surveyWave) %>%
       #spread("surveyWave", value = c("Wave 1", "Wave 2")) %>% 
       kable("html") %>% 
       kable_styling("striped") 
       #group_rows("Educational Resources", 4, 7) %>%
       #group_rows("Financial Resources", 8, 10) %>% 
       #group_rows("Tools and Equipment", 8, 10) %>% 
       #group_rows("Chemical Inputs", 4, 7) %>%
       #group_rows("Extension Services ", 8, 10)
   }
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

