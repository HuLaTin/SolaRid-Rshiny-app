library(plotly)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(ggplot2)
library(shiny)

# Define UI
options(shiny.maxRequestSize = 150*1024^2)
ui <- fluidPage(theme = shinytheme("flatly"),

                # Application title
                titlePanel("Ozone Sensors x Time -- TJWatkins"),

                # Sidebar with a slider input for number of bins
                sidebarLayout(
                    sidebarPanel(
                        fileInput(inputId = "file1", label = "Upload CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values, text/plain",
                                             ".csv")

                        ),
                        width = 2
                    ),

                    mainPanel(
                        tabsetPanel( type = "tabs",
                                     tabPanel(
                                         titlePanel("Plot"),
                                         plotlyOutput("sensorPlot", height = 720)
                                         #fix height fluidpage or fillpage?
                                     ),
                                     tabPanel(
                                         titlePanel("Summary"),
                                         verbatimTextOutput("summary")
                                     )

                        ),
                        width = 10
                    )
                )
)



# Define server logic
server <- function(input, output, session) {

    data <- reactive({

        req(input$file1)

        df <- read.csv(input$file1$datapath,
                       header = FALSE,
                       sep = ",")

        colnames(df, do.NULL = FALSE)
            colnames(df) <- c("Time", "A", "B")
        #should remove nulls per scan error
        df <- df[!(is.na(df$Time) | df$Time==""), ]

        #set Time to POSIX
        df$Time <- as.POSIXct(df$Time, origin="1970-01-01", tz="GMT")

        rownames(df) <- seq(length=nrow(df))

        print(df)
        df

    })

    #output$Plot <- renderPlot({
    output$sensorPlot <- renderPlotly({

        sensorPlot <- plot_ly(data(), x = ~Time, y = ~A, name = 'A', type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~B, name = 'B', mode = 'lines') %>%

            layout(xaxis = list(title = 'Time'),
                   yaxis = list (title = 'Signal'))
    })

    output$summary <- renderPrint({
        y <- data()
        summary(y)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
