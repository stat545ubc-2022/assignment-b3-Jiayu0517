library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  ## Feature 1: Added a BC Liquor Store image
  img(src="download.png", height="15%", width="15%"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      ## Feature 2: sort data by alcohol content
      checkboxInput("sortInput", "Sort results by alcohol content", TRUE),
      uiOutput("countryOutput")
    ),
    mainPanel(
      ## Feature 3: User can download the data table to local machine
      downloadLink('downloadData', 'Click to download the table'),

      ## Feature 4: Modified tab panel to contain 2 tabs for plot and table
      tabsetPanel(
        tabPanel("Plot", plotOutput("coolplot")),
        tabPanel("Table", tableOutput("results"))
      )
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    random <-
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )

    # Feature 2: sort table by alcohol content
    if (input$sortInput) {
      random %>%
        arrange(Alcohol_Content)
    } else {
      random
    }

  })

  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- renderTable({
    filtered()
  })

  ## Feature 3: download table to local machine
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-BCliquor.csv', sep='')
    },
    content = function(con) {
      write.csv(filtered(), con)
    }
  )
}

shinyApp(ui = ui, server = server)
