library(tidyverse)
library(ggmap)
library(shiny)
library(ggplot2)

# import dataset
crime_data <- read_csv("http://library.metatab.org/sandag.gov-crime-2007e2013-2.1.4/data/incidents.csv")


# First, install and load the necessary packages
install.packages(c("shiny", "ggplot2"))
library(shiny)
library(ggplot2)

# Next, create the user interface for the Shiny app
ui <- fluidPage(
  titlePanel("Crime Hexbin Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("desc", "Crime desc:",
                  c("All", unique(crime_data$desc)),
                  selected = "All")
    ),
    mainPanel(
      plotOutput("hexbin_plot")
    )
  )
)

# Next, create the server function for the Shiny app
server <- function(input, output) {
  
  # Create a reactive data subset based on the selected crime desc
  crime_data_subset <- reactive({
    if (input$desc == "All") {
      return(crime_data)
    } else {
      return(crime_data[crime_data$desc == input$desc,])
    }
  })
  
  # Create the hexbin plot
  output$hexbin_plot <- renderPlot({
    ggplot(data = crime_data_subset(), aes(x = lon, y = lat)) + 
      geom_hex(bins = 30) +
      scale_fill_gradient(low = "white", high = "red") +
      ggtitle(input$desc)
  })
}

# Finally, run the Shiny app
shinyApp(ui = ui, server = server)