#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(htmltools)
data <- read.delim("UAH-lower-troposphere-long.csv.bz2")
data


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UAH"),
    

    
    tabsetPanel(
      tabPanel("About", 
               p("This is the content of Tab 1"),
               p("second line")),
      tabPanel("Plots", 
               column(width = 3,
                      checkboxGroupInput("regions", "Select regions", choices = unique(data$region), selected = "globe"),
                      checkboxInput("trendline", "Trendline", "Display trend line", value = FALSE)
                      ),
               mainPanel(plotOutput("scatterplot"))
               ),
      tabPanel("Tables", "This is the content of Tab 3")
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Filter data based on selected regions
  filtered_data <- reactive({
    data %>% filter(region %in% input$regions)
  })
  
  # Create scatterplot with filtered data
  output$scatterplot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = year, y = temp, color = region)) +
      geom_point() +
      labs(x = "Year", y = "Temperature", color = "Region")
    
    if (input$trendline) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
