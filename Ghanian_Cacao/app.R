
library(shiny)
library(leaflet)
library(knitr)

#should read in read_rds(path = "ENTER FILE NAME")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  # NOTE TO SELF: ALIGN THIS CENTER
   titlePanel("Ghanian Cacao Farmers"),
   
   # Authorship
   h4("Analysis and visualizations by Molly Leavens"),
   
   # Short description of survey
   p("This app summarizes some of the key variables for a survey of several thousand 
      cacao farmers across Ghana in both 2008 and 2014. Harvard Professor Micheal 
      Hiscox and Mondelez, the world’s largest chocolate company, jointly implemented 
      this survey."),
   
   # Sidebar with action buttons
   sidebarLayout(
      sidebarPanel(
        #turn this into a drop-down box 
        #add option where you can color by gender
        #add option to separate by region ()
         h5("Click on varible to see the distribution of this variable"),
         actionButton("gghhsize", "House Hold Size"),
         actionButton("ggfraclost", "Fraction of Harvest Lost to Problems"),
          hr()
      ),
      
      mainPanel(
        
        # Show plots
         plotOutput("boxPlot"),
         
        # Show map of survey distribution 
         h5("Distribution of Survey locations"),
         leafletOutput("surveymap"),
        
        # Show knitr tables 
         h5("Percentage of respondents with access to key resources"),
         tableOutput("ed_resources"),
         tableOutput("fin_resources"),
         tableOutput("chem_resources"),
         tableOutput("phys_resources")
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

