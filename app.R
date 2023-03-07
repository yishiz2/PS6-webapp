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
               p("This app uses satellite temperature data from *UAH*"),
               p("Temperature temp is measured as deviation (deg C) from 1991-2020 baseline"),
               dataTableOutput("sample")
               ),
      tabPanel("Plots", 
               column(width = 3,
                      checkboxGroupInput("regions", "Select regions", choices = unique(data$region), selected = "globe"),
                      textOutput("Display trendline"),
                      checkboxInput("trendline", "Display trend line", value = TRUE),
                      selectInput("color_palette", "Select color palette", choices = c("Default" = "Default", "Plasma" = "plasma"), selected = "")
                      ),
               mainPanel(plotOutput("scatterplot"))
               ),
      tabPanel("Tables",
               sidebarLayout(
                 sidebarPanel(
                   p("This panel displays average temperature over months and years"),
                   br(),
                   radioButtons("Time",
                                "Average time period:",
                                choices = c("year" = "year", "month" = "month")),
                   br(),
                 ),
                 mainPanel(
                   textOutput("temp"),
                   dataTableOutput("table")
                 )
               )
              )
      )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sample <- renderDataTable({
    data %>%
      sample_n(6)
  })

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
    
    if (!is.null(input$color_palette) && input$color_palette != "") {
      p <- p + scale_color_viridis_d()
    }
    
    p
  
  })
  
  output$table <- renderDataTable({
    filtered_data()
  })
  output$temp <- renderText({
    if (input$Time == "year") {
      paste("Temperature data for year")
    } else {
      paste("Temperature data for month")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
