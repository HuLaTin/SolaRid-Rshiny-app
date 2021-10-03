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
                titlePanel("MQ Sensors x Time (Joules)"),

                # Sidebar with a slider input for number of bins
                sidebarLayout(
                    sidebarPanel(
                        fileInput(inputId = "file1", label = "Upload CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values, text/plain",
                                             ".csv"),

                        ),
                        radioButtons("radio", "Normalize?",
                                     c("raw" = "raw",
                                       "normalize" = "norm",
                                       "remove min (test)" = "mins")),

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

        radio <- input$radio

        req(input$file1)

        #add a check box for if then, that would allow you to choose if you want to normalize data?
        normalize <- function(x)
        {
            return ((x - min(x)) / (max(x) - min(x)))
        }

        df <- read.csv(input$file1$datapath,
                       header = FALSE,
                       stringsAsFactors = FALSE,
                       sep = ",")

        colnames(df, do.NULL = FALSE)

        # when, mq2sensorValue, LPGppm,
        # mq4sensorValue, CH4ppm, mq5sensorValue,
        # MQ5LPGppm, mq6sensorValue,
        # MQ6LPGppm, mq7sensorValue, H2ppm,
        # mq8sensorValue, MQ8H2ppm,
        # mq9sensorValue, COppm, mq3sensorValue,
        # Uppm, bme680.temperature, bme680.gas,
        # bme680.humidity,bme680.pressure,
        # load,throttle

        colnames(df) <- c("Time", "MQ2_ADC", "MQ3_ADC", "MQ4_ADC", "MQ5_ADC",
                                  "MQ6_ADC", "MQ7_ADC", "MQ8_ADC", "MQ9_ADC",
                                  "TempC", "Gas_ohms", "Humidity",
                                  "Pressure_pa", "CPU_Load", "Throttled")


        #df <- df[ -c() ]
        #print(head(df))

        #should remove nulls per scan error
        df <- df[!(is.na(df$Time) | df$Time==""), ]

        #set Time to POSIX
        df$Time <- as.POSIXct(df$Time, origin="1970-01-01", tz="GMT")

        if (radio == "norm"){
            df[,2:ncol(df)] <- as.data.frame(lapply(df[,2:ncol(df)], normalize))
        }
        if (radio == "mins"){
            for (i in 2:ncol(df))
            {
                colmin <- min(df[,i])
                for (row in 1:nrow(df))
                {
                    df[row,i] <- (df[row,i] - colmin)
                }
            }
        }
        rownames(df) <- seq(length=nrow(df))

        print(head(df))
        df

    })

    #output$Plot <- renderPlot({
    output$sensorPlot <- renderPlotly({

        sensorPlot <- plot_ly(data(), x = ~Time, y = ~MQ2_ADC, name = 'MQ2', type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~MQ3_ADC, name = 'MQ3', mode = 'lines') %>%
            add_trace(y = ~MQ4_ADC, name = 'MQ4', mode = 'lines') %>%
            add_trace(y = ~MQ5_ADC, name = 'MQ5', mode = 'lines') %>%
            add_trace(y = ~MQ6_ADC, name = 'MQ6', mode = 'lines') %>%
            add_trace(y = ~MQ7_ADC, name = 'MQ7', mode = 'lines') %>%
            add_trace(y = ~MQ8_ADC, name = 'MQ8', mode = 'lines') %>%
            add_trace(y = ~MQ9_ADC, name = 'MQ9', mode = 'lines') %>%
            add_trace(y = ~TempC, name = 'Temp *C', mode = 'lines', visible = "legendonly") %>%
            add_trace(y = ~Gas_ohms, name = 'Gas ohms', mode = 'lines', visible = "legendonly") %>%
            add_trace(y = ~Humidity, name = 'Humidity', mode = 'lines', visible = "legendonly") %>%
            add_trace(y = ~Pressure_pa, name = 'Pressure', mode = 'lines', visible = "legendonly") %>%
            #add_trace(y = ~CPU_Load, name = 'CPU Load', mode = 'lines', visible = "legendonly") %>%


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
