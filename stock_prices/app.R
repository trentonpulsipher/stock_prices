
library(shiny)
library(Quandl)
  Quandl.api_key("1xrGSwDRn_MapbqUyt2x") # my personal free key
library(tidyverse)
library(lubridate)
library(rbokeh)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
  

# function to grab the data from Quandl
get_raw_data <- function(ticker) {
  # reminder: my Quandl API_key is hard coded below
  url <- paste0("https://www.quandl.com/api/v3/datasets/WIKI/", ticker, "/data.csv?collapse=daily&column_index=4&api_key=1xrGSwDRn_MapbqUyt2x")
  system(paste0('curl ', url), intern = T)
}

# function to forecast using random-walk
random_walk <- function(lastPrice = data2use() %>% pull(Close) %>% last(),
                        lastDate = data2use() %>% pull(Date) %>% max(na.rm = T),
                        days = 30, #input$numDaysForecast
                        drift = 0,
                        sig = 1,
                        numMCs = 500) {
  out <- list()
  for(i in 1:numMCs) {
    out[[i]] <- rep(NA, length = days + 1)
    out[[i]][1] <- lastPrice
    for(j in 2:(days+1)) {
      out[[i]][j] = out[[i]][j-1] + rnorm(1, 0, sig) + drift
    }
  }
  data.frame(Date = lastDate + 1:days,
             Forecast = apply(do.call(rbind,out)[,-1], 2, quantile, probs = 0.5),
             CI95LB = apply(do.call(rbind,out)[,-1], 2, quantile, probs = 0.05),
             CI95UB = apply(do.call(rbind,out)[,-1], 2, quantile, probs = 0.95)) %>%
    as.tibble()
}


## Grab All Tickers available for free from Quandl
# allTickers <- system('curl "https://www.quandl.com/api/v3/databases/WIKI/codes?api_key=1xrGSwDRn_MapbqUyt2x"', inter = T)
allTickers <- read_csv("secwiki_tickers.csv") %>%
  filter(str_detect(Price, "WIKI"))



# Define UI for application 
ui <- fluidPage(
   # App title
   titlePanel("Stock Prices"),
   # Sidebar with a text input for ticker
   sidebarLayout(
      sidebarPanel(
         selectInput("ticker",
                     "Stock Ticker:",
                     choices = allTickers %>% select(Ticker),
                     selected = "AAPL"),
         radioButtons("method",
                      "Forecast Method",
                      choices = c("ETS time-series", "Random Walk"),
                      selected = "Random Walk"),
         numericInput("numDaysForecast",
                     "Number of Days to Forecast:",
                     value = 30,
                     min = 5,
                     max = 365,
                     step = 5),
         tableOutput("tickerInfo"),
         textOutput("description")
      ),
      # Show rBokeh plot of time-series and Forecast and Data
      mainPanel(
        tabsetPanel(
          tabPanel("Historical", rbokehOutput("tsPlot")),
          tabPanel("Forecast", plotOutput("forecast")),
          tabPanel("Data Table", DT::dataTableOutput("dataTable"))
        )
      )
   )
)

# Define server logic required to render plots/tables
server <- function(input, output) {
  output$tickerInfo <- renderTable(allTickers %>% 
                                    filter(Ticker == input$ticker) %>%
                                    select(Name, Sector, Industry))
  output$description <- renderText(
    "DESCRIPTION of WIKI data
    https://www.quandl.com/databases/WIKIP
    End of day stock prices, dividends and splits for 3,000 US companies, curated by the Quandl community and released into the public domain."
  )
  # grab the raw data from Quandl
  data2use <- reactive({
    get_raw_data(input$ticker) %>%
    as.tibble() %>%
    slice(-1) %>% # remove the header line
    mutate(
      Date = ymd(unlist(lapply(str_split(value, ","), "[[", 1))),
      Close = as.numeric(unlist(lapply(str_split(value, ","), "[[", 5))),
      Volume = as.numeric(unlist(lapply(str_split(value, ","), "[[", 6)))
    ) %>%
    select(Date, Close, Volume) %>%
    arrange(desc(Date))
  })
  
  output$tsPlot <- renderRbokeh({
    data2use() %>%
      figure(xlab = "", ylab = "Price at Closing (Daily)") %>%
      ly_points(Date, Close, 
                hover = list("Date" = Date, "Closing Price" = Close), 
                alpha = 0.25,
                size = 5)
  })
  
  output$forecast <- renderPlot({
    if(input$method == "ETS time-series") {
      # create the forecast
      forecastData <- data2use() %>% 
        # use the last 3 years for prediction
        filter(Date > (max(Date, na.rm = T) - years(3))) %>%
        # convert from tibble to ts structure
        tk_ts(select = Close) %>%
        # Exponential smoothing (error, trend, seasonal) model
        ets() %>%
        forecast(h = input$numDaysForecast) %>%
        # back to tibble
        tk_tbl(timetk_idx = TRUE) %>%
        # convert back to date
        mutate(Date = max(data2use()$Date) + days(1:input$numDaysForecast)) %>%
        rename(Forecast = `Point Forecast`,
               CI95LB = `Lo 95`,
               CI95UB = `Hi 95`) %>%
        select(Date, Forecast, CI95LB, CI95UB)
    } 
    if(input$method == "Random Walk") {
      tmp <- data2use()
      numObs <- tmp %>% 
        filter(Date > (max(Date, na.rm = T) - years(3))) %>%
        pull(Date) %>%
        length()
      # calculate the stdev for the random walk
      sig <- tmp %>% 
        filter(Date > (max(Date, na.rm = T) - years(1))) %>% 
        pull(Close) %>% 
        sd(na.rm = T) #/ (numObs)
      
      forecastData <- random_walk(lastPrice = tmp %>% arrange(Date) %>% pull(Close) %>% last(),
                                  days = input$numDaysForecast,
                                  drift = 0,
                                  sig = sig,
                                  numMCs = 1000,
                                  lastDate = tmp %>% pull(Date) %>% max(na.rm = T))
    }
    data2use() %>%
      filter(Date > (max(Date, na.rm = T) - years(3))) %>%
      ggplot(aes(x = Date, y = Close)) +
        geom_point(color = "blue", alpha = 0.5) +
        geom_ribbon(aes(x = Date, y = Forecast, ymin = CI95LB, ymax = CI95UB),
                    data = forecastData, fill = "red", alpha = 0.5) +
        geom_line(aes(x = Date, y = Forecast), data = forecastData, color = "red") +
        theme_bw() +
        labs(x = "", 
             y = "Price at Closing (Daily)", 
             title = "Forecast (red) with 95% Confidence Interval Bands")
  })
  #   renderRbokeh({
  #   data() %>%
  #     filter(Date > (max(Date, na.rm = T) - months(3))) %>%
  #     figure(xlab = "", ylab = "Price at Closing (Daily") %>%
  #     ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close))
  # })
  output$dataTable <- DT::renderDataTable({
    tmp <- data2use() # %>%
      # filter(Date > (max(Date, na.rm = T) - years(3))) %>%
      # left_join(forecastData() %>% arrange(desc(Date)), by = "Date")
    DT::datatable(tmp)
  })#, options = list(pageLength = 10)))
}

# Run the application 
shinyApp(ui = ui, server = server)

# library(rsconnect)
# rsconnect::deployApp('~/Documents/Development/R/stock_prices/stock_prices')

## NOTES:
# https://www.quandl.com/databases/WIKIP
# DESCRIPTION of WIKI data: End of day stock prices, dividends and splits for 3,000 US companies, curated by the Quandl community and released into the public domain.
# could vary more than just closing price
# could provide shorter window (say last year) if desired
# columns of data as it comes from Qunadl: c("Date", "Open", "High", "Low", "Close", "Volume", "Ex-Dividend", "Split Ratio", 
# "Adj. Open", "Adj. High", "Adj. Low", "Adj. Close", "Adj. Volume")

