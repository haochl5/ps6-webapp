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

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "General Information",
      h1("Investigation on global temperature"),
      p("This global temperature dataset is collected from ",strong("UAH")),
      p("This dataset records temperature measured as degree Celcius at specific",em("time and region")),
      p("Dataset includes temperature data from 1991 to 2020"),
      p("Number of rows in this dataset: "),
      uiOutput("rowNum"),
      p("Number of columns in this dataset: "),
      uiOutput("colNum"),
      verbatimTextOutput("summary"),
      dataTableOutput("sample")
    ),
    tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          h2("Plot modification"),
          p("In this app, you can modify the graph to analysis the global temperature for different regions.
            Select the regions you are interested in and the color for the scatterplot."),
          fluidRow(
            column(6,
                   radioButtons("color", "Choose color of the graph",
                                choices = c("blue","black","red","purple","yellow"))),
            column(6,
                   uiOutput("select")
            ),
            column(6,
                   checkboxInput(
                     "trend",
                     "Show Trend Line"
                   ))
          ),
        ),
        mainPanel(
          plotOutput("plot"),
          verbatimTextOutput("textualOutput"),
          verbatimTextOutput("range")
        )
      )
    ),
    tabPanel(
      "Table",
      sidebarLayout(
        sidebarPanel(
          h2("Table"),
          p("This table shows average temperature over month or year."),
          radioButtons("time", "Average over: ", 
                       c("month","year")
        )),
        mainPanel(
          h2("Panel of Average Temperature"),
          verbatimTextOutput("info"),
          verbatimTextOutput("range2"),
          tableOutput("table")
        )
      )
    )
  )
)

server <- function(input, output) {
    temperature <- read_delim("UAH-lower-troposphere-long.csv.bz2")
  
    output$select <- renderUI({
      selectInput("region", "Choose region to display",
                         choices = unique(temperature$region))
    })
    
    sample <- sample_n(temperature, 8)
    
    output$summary <- renderText({
      "Here is a small sample of data:"
    })
    
    output$rowNum <- renderUI({
      p(paste(nrow(temperature)))
    })
    
    output$colNum <- renderUI({
      p(paste(ncol(temperature)))
    })
    
    output$sample <- renderDataTable ({
      sample
    })
    
    graphData <- reactive({
      s1 <- temperature %>%
        group_by(year, region) %>%
        mutate(meanTemp = mean(temp)) %>%
        filter(region == input$region)
        return(s1)
    })
    
    output$textualOutput <- renderText({
      paste("The maximum average temperature is ", round(max(graphData()$meanTemp),3), ", the minimum average temperature is ", round(min(graphData()$meanTemp),3))
    })
    
    output$range <- renderText({
      paste("The range between maximum and minimum is ", round(max(graphData()$meanTemp)-min(graphData()$meanTemp),3))
    })
    
    output$plot <- renderPlot({
      ggplot(data = graphData(), mapping = aes(x = year,y = meanTemp)) + 
        ggtitle(paste("Average temperature in", input$region)) +
        geom_point(color = input$color) +
        if (input$trend) {
          geom_smooth(method = lm)
        } else {
          geom_blank()
        }
    })
    
    tableData <- reactive ({
      if (input$time == "month"){
        filtered <- temperature %>%
          group_by(year, month) %>%
          summarise(averageTemperature = mean(temp))
      } else if (input$time == "year") {
        filtered <- temperature %>%
          group_by(year) %>%
          summarise(averageTemperature = mean(temp))
      }
    })
    
    output$info <- renderText({
      paste("The maximum average temperature is ", round(max(tableData()$averageTemperature),3), ", the minimum average temperature is ", round(min(tableData()$averageTemperature),3))
    })
    
    output$range2 <- renderText({
      paste("The range between maximum and minimum temperature is ", round(max(tableData()$averageTemperature)-min(tableData()$averageTemperature),3))
    })
    
    output$table <- renderTable({
      tableData()
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
