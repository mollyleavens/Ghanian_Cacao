
library(readr)
library(shiny)
library(leaflet)
library(knitr)
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
                     h4("Analysis and visualizations by Molly Leavens"))
        

)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  variableInput <- reactive({
    switch(input$variable,
           "House Hold Size" = both_waves$gghhsize, 
           "Number of Children" = both_waves$numChildren, 
           "Age of House Hold Head" = both_waves$ggmeanage)
  })

#render map
   output$surveymap <- renderLeaflet ({
     leaflet() %>%
     addTiles() %>%  
     addCircles(lng=both_waves$l_gpslongitude, 
                lat=both_waves$l_gpslatitude, 
                weight = 1, radius = 1)
   })
   
#render plot 
   output$boxPlot <- renderPlot({
       ggplot(both_waves, aes_string(variableInput())) + geom_bar() + labs(x = input$variable)
     })
   
#render percentage tables 
  
#tableOutput("phys_resources"))
    
   output$phys_resources <- renderTable({  
   phys_resources
   }) 
   #colnames = c("Car", "Chainsaw", "Mistblower", "Electricity"))
   
   output$chem_resources <- renderTable({  
     chem_resources
   }) 
  
   output$fin_resources <- renderTable({  
     fin_resources
   })
   
   output$ed_resources <- renderTable({  
     ed_resources
   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

