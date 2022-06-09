library(plotly)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(ggplot2)
library(shiny)
#library(tidyverse)


# Define UI
options(shiny.maxRequestSize = 150*1024^2)
ui <- fluidPage(theme = shinytheme("simplex"),
                
                # Application title
                titlePanel("BirdHouse"),
                
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
                                   "smooth" = "smoove",
                                   "normalize" = "norm")),
                    
                    width = 4
                  ),
                  
                  mainPanel(
                    tabsetPanel( type = "tabs",
                                 tabPanel(
                                   titlePanel("Plot"),
                                   plotlyOutput("birdHousePlot", height = 1440)
                                   #fix height fluidpage or fillpage?
                                 ),
                                 tabPanel(
                                   titlePanel("Summary"),
                                   verbatimTextOutput("summary")
                                 )
                                 
                    ),
                    width = 100
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
    
    colnames(df) <- c('DateTime','MQ5_1','MQ4_1','MQ5_2','MQ4_2','uv415nm','uv445nm',
                      'uv480nm','uv515nm','uv555nm','uv590nm','uv630nm','uv680nm','BME_gas',
                      'BME_pressure','BME_temp','BME_humidity','UV_light','UV_Index',
                      'Amb_light','LUX')
    
    #set Time to POSIX
    df$DateTime <- as.POSIXct(df$DateTime, origin="1970-01-01", tz="GMT")
    
    #gasDf
    #df <- df[,c('DateTime','MQ5_1','MQ4_1','MQ5_2','MQ4_2')]    
    
    
    #df <- df[ -c() ]
    #print(head(df))
    
    #should remove nulls per scan error
    #df <- df[!(is.na(df$Time) | df$Time==""), ]
    
    if (radio == "norm"){
      df[,2:ncol(df)] <- as.data.frame(lapply(df[,2:ncol(df)], normalize))
    }
    
    if (radio == "smoove"){
      for (row in 3:(nrow(df)-2))
      {
        startRow <- row-2
        endRow <- row+2

        
        winDF[row,i] <- mean(df[startRow:endRow,i])
        
      }
      
      df <- winDF
      
    }
    
    rownames(df) <- seq(length=nrow(df))
    
    #print(head(df))
    df
    
  })
  
  #output$Plot <- renderPlot({
  output$birdHousePlot <- renderPlotly({

    birdHouseGas <- plot_ly(data(), x = ~DateTime, y = ~MQ5_1, name = 'MQ5_1', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~MQ4_1, name = 'MQ4_1', mode = 'lines') %>%
      add_trace(y = ~MQ5_2, name = 'MQ5_2', mode = 'lines') %>%
      add_trace(y = ~MQ4_2, name = 'MQ4_2', mode = 'lines') %>%
      
      add_annotations(text = "Gas",
                      x = 0,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "left",
                      yanchor = "top",
                      yshift = 20,
                      showarrow = FALSE,
                      font = list(size = 15)) %>%

      
      layout(xaxis = list(title = 'DateTime'),
             yaxis = list (title = 'Signal'))
    
    #'415nm','445nm','480nm','515nm','555nm','590nm','630nm','680nm'
    birdHouseUV <- plot_ly(data(), x = ~DateTime, y = ~uv415nm, name = '415nm', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~uv445nm, name = '445nm', mode = 'lines') %>%
      add_trace(y = ~uv480nm, name = '480nm', mode = 'lines') %>%
      add_trace(y = ~uv515nm, name = '555nm', mode = 'lines') %>%
      add_trace(y = ~uv555nm, name = '555nm', mode = 'lines') %>%
      add_trace(y = ~uv590nm, name = '630nm', mode = 'lines') %>%
      add_trace(y = ~uv630nm, name = '630nm', mode = 'lines') %>%
      add_trace(y = ~uv680nm, name = '680nm', mode = 'lines') %>%
      
      add_annotations(text = "UV",
                      x = 0,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "left",
                      yanchor = "top",
                      yshift = 20,
                      showarrow = FALSE,
                      font = list(size = 15)) %>%
      
      layout(xaxis = list(title = 'DateTime'),
             yaxis = list (title = 'Signal'))
    
    #'BME_gas','BME_pressure','BME_temp','BME_humidity'
    birdHouseBME <- plot_ly(data(), x = ~DateTime, y = ~BME_pressure, name = 'BME_pressure', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~BME_temp, name = 'BME_temp', mode = 'lines') %>%
      add_trace(y = ~BME_humidity, name = 'BME_humidity', mode = 'lines') %>%
      add_trace(y = ~BME_gas, name = 'BME_gas', mode = 'lines', visible = "legendonly") %>%
      
      add_annotations(text = "BME",
                      x = 0,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "left",
                      yanchor = "top",
                      yshift = 20,
                      showarrow = FALSE,
                      font = list(size = 15)) %>%
      
      layout(xaxis = list(title = 'DateTime'),
             yaxis = list (title = 'Signal'))
      
    #'UV_light','UV_Index','Amb_light','LUX'
    birdHouseUVInd <- plot_ly(data(), x = ~DateTime, y = ~UV_light, name = 'UV_light', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~Amb_light, name = 'Amb_light', mode = 'lines') %>%
      add_trace(y = ~LUX, name = 'LUX', mode = 'lines') %>%
      add_trace(y = ~UV_Index, name = 'UV_Index', mode = 'lines', visible = "legendonly") %>%
      
      add_annotations(text = "UV_LUX",
                      x = 0,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "left",
                      yanchor = "top",
                      yshift = 20,
                      showarrow = FALSE,
                      font = list(size = 15)) %>%
      
      layout(xaxis = list(title = 'DateTime'),
             yaxis = list (title = 'Signal'))
    

    
    subplot(list(birdHouseGas, birdHouseUV, birdHouseBME, birdHouseUVInd), nrows = 4, margin = 0.06)

  })
  
  output$summary <- renderPrint({
    y <- data()
    summary(y)
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)
