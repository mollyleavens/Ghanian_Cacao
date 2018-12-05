
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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  # NOTE TO SELF: ALIGN THIS CENTER
   titlePanel("Ghanian Cacao Farmers"),
   
   # Authorship
   h4("Analysis and visualizations by Molly Leavens"),
   
   # Sidebar with action buttons
   sidebarLayout(
      #sidebarPanel(
        #turn this into a drop-down box 
        #add option where you can color by gender
        #add option to separate by region ()
        # provide instructions h5("Click on varible to see the distribution of this variable"),
         #actionButton("gghhsize", "House Hold Size"),
         #actionButton("ggfraclost", "Fraction of Harvest Lost to Problems"),
         #tabs for wave 1 and 2 or both
         #conditionalPanel(condition = "tabPanel == `Box Plots`",
         
         
          hr(),
      
      
      # Main Panel with Tabs (1 for each gender)
      # Output plots  
      mainPanel(
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Survey Overview",    
                             # Short description of survey
                             p("Ghana is the world’s second largest cacao producing region after its neighbor 
                               the Ivory Coast. The social, economic, and environmental conditions of these 
                               farmers is therefore critical in assessing effective intervention methods and 
                               predicting future global cacao market trends. 
        
                               This app summarizes some of the key variables for a survey of several thousand 
                               cacao farmers across Ghana in both 2008 and 2014. Harvard Professor Micheal 
                               Hiscox and Mondelez, the world’s largest chocolate company, jointly implemented 
                               this survey. This app does not contain statistical analyses and is not meant to 
                               claim correlations, causations, or significance. It is rather to display the 
                               framework for the survey and some key aggregated data.")),
       
             # Show map of survey distribution 
                    tabPanel("Map", 
                             h5("Distribution of Survey locations"),
                             p("Each map point represents a single conducted survey"),
                             leafletOutput("surveymap")),
                    
             # Show plots
                    tabPanel("Box Plots", 
                             selectInput(inputId = "variable",
                                         label = "Choose a variable:",
                                         choices = c("House Hold Size", "Number of Children", "Age of House Hold Head",
                                                     "")),
                            plotOutput("boxPlot")),
             
             # Show knitr tables       
                    tabPanel("Summary Datatables",  
                             h5("Percentage of respondents with access to key resources"),
                             tableOutput("ed_resources"),
                             tableOutput("fin_resources"),
                             tableOutput("chem_resources"),
                             tableOutput("phys_resources"))
             
             , id = "conditionedPanels"
            
              )
        
        
      )
   )
)
   


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  v <- reactiveValues(data = NULL)
   
   observeEvent(input$gghhsize, {
     v$data <- (both_waves$gghhsize)
   })
   
   observeEvent(input$ggfraclost, {
     v$data <- (both_waves$ggfraclost)
   })

#render map
#add title    
   output$surveymap <- renderLeaflet ({
     leaflet() %>%
     addTiles() %>%  
     addCircles(lng=both_waves$l_gpslongitude, 
                lat=both_waves$l_gpslatitude, 
                weight = 1, radius = 1)
   })
   
#render plot 
   output$boxPlot <- renderPlot({
       if (is.null(v$data)) return()
       boxplot(v$data)
       ##(ggplot(v$data) + geom_bar() + labs(title = "Distribution of House Hold Size"))
     })
   
#render percentage tables 
   output$phys_resources <- renderTable({
     #req(input$)
     (phys_resources %>% 
     kable(caption = "Percentage of respondants with access to physical resources",
          col.names = c("Car", "Chainsaw", "Mistblower", "Electricity")))
   })
  # fin_resources %>% 
    # kable(caption = "Total number of unique candidate IDs and comittee IDs for each party",
    #       col.names = c("Party","Candidate IDs", "Committee IDs"))
   
#   chem_resources %>% 
#     kable(caption = "Total number of unique candidate IDs and comittee IDs for each party",
 #          col.names = c("Party","Candidate IDs", "Committee IDs"))
   
}

# Run the application 
shinyApp(ui = ui, server = server)

