#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse,
               sf,
               XML,
               tmap,
               viridis, 
               aws.s3,
               rio,
               janitor,
               shiny,
               tmaptools)




# Load data ---------------------------------------------------------------

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

lsoa_shp<-s3read_using(readRDS # Which function are we using to read
                       , object = 'lsoa_cdrc.RDS' # File to open
                       , bucket = buck) # Bucket name defined above



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CDRC maps"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("in_year", label = "Year",
                      choices =paste0("chn",c(1997:2019)), selected = "chn1997")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tmapOutput("map1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map1 <- renderTmap({
      tm_shape(lsoa_shp) +
        tm_borders(,alpha=0) +
        tm_fill(col = input$in_year, style="cont", palette = "viridis", 
                title = paste0("Churn index", substr(input$in_year,4,7))) +
        tm_layout(legend.title.size = 0.8,
                  legend.text.size = 0.6,
                  legend.position = c("left","top"),
                  legend.bg.color = "white",
                  legend.bg.alpha = 1) 
      
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
