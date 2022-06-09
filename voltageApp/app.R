library(plotly)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(ggplot2)
library(shiny)

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),

                # Application title
                titlePanel("KillingGrid Readings"),

                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = "file1", label = "Upload CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values, text/plain",
                                         ".csv")
                    ),
                    radioButtons("radio", "Normalize?",
                                 c("raw" = "raw",
                                   "smooth" = "smoove",
                                   "normalize" = "norm")),
                    numericInput("numInput", "Moving Average Window", 10,
                                 min = 1, max = 100),
                    width = 4
                  ),

                  mainPanel(
                    tabsetPanel( type = "tabs",
                                 tabPanel(
                                   titlePanel("Plot"),
                                   plotlyOutput("sensorPlot", height = 720)
                                   #textOutput("my_csv_name")

                                   #fix height fluidpage or fillpage?
                                 ),
                                 tabPanel(
                                   titlePanel("Summary"),
                                   verbatimTextOutput("summary")
                                 )
                                 # tabPanel(
                                 #   titlePanel("Time"),
                                 #   verbatimTextOutput("time")
                                 # )

                    ),
                    width = 20
                  )
                )
)


# Define server logic
server <- function(input, output, session) {

  data <- reactive({

    radio <- input$radio
    win <- input$numInput

    req(input$file1)

    normalize <- function(x)
    {
      return ((x - min(x)) / (max(x) - min(x)))
    }

    df <- read.csv(input$file1$datapath,
                   header = FALSE,
                   sep = ",")

    if (radio == "norm"){
      df[,1:ncol(df)] <- as.data.frame(lapply(df[,1:ncol(df)], normalize))
    }
    
    if (radio == "smoove"){
      winDF <- df
      win1 = win - 1
      for (i in 1:ncol(df))
      {
        for (row in win:(nrow(df)-win1))
        {
          startRow <- row-win1
          endRow <- row+win1
        
          winDF[row,i] <- mean(df[startRow:endRow,i])
        
        }
      }
      winDF <- winDF[win:(nrow(winDF)-win1),]
      df <- winDF
      
    }

    df <- as.data.frame(df)
    #print(head(df))

    count <- (ncol(df) + 1)
    #print(count)

    df[,count:8] = NA
    #print(head(df))

    #df <- df[complete.cases(df), ]

    colnames(df, do.NULL = FALSE)
    colnames(df) <- c("A", "B", "C", "D", "E", "F", "G", "H")

    df$index <- seq.int(nrow(df))

    rownames(df) <- seq(length=nrow(df))

    #print(head(df))
    df
  })

  output$sensorPlot <- renderPlotly({

    sensorPlot <- plot_ly(data(), x = ~index, y = ~A, name = 'A', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~B, name = 'B', mode = 'lines') %>%
      add_trace(y = ~C, name = 'C', mode = 'lines') %>%
      add_trace(y = ~D, name = 'D', mode = 'lines') %>%
      add_trace(y = ~E, name = 'E', mode = 'lines') %>%
      add_trace(y = ~F, name = 'F', mode = 'lines') %>%
      add_trace(y = ~G, name = 'G', mode = 'lines') %>%
      add_trace(y = ~H, name = 'H', mode = 'lines') %>%
      layout(xaxis = list(title = 'Index'),
             yaxis = list (title = 'Raw A-D converted'))
  })

  output$summary <- renderPrint({
    y <- data()
    print(summary(y))


  })

  output$my_csv_name <- renderText({
    # Test if file is selected
    if (!is.null(input$x$datapath)) {
      # Extract file name (additionally remove file extension using sub)
      return(sub(".csv$", "", basename(input$x$name)))
    } else {
      return(NULL)
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
