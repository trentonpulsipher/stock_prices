
library(shiny)
library(Quandl)
  Quandl.api_key("1xrGSwDRn_MapbqUyt2x") # my personal free key
library(tidyverse)
library(lubridate)
library(rbokeh)

# function to grab the data from Quandl
get_raw_data <- function(ticker) {
  # reminder: my Quandl API_key is hard coded below
  url <- paste0("https://www.quandl.com/api/v3/datasets/WIKI/", ticker, "/data.csv?collapse=daily&column_index=4&api_key=1xrGSwDRn_MapbqUyt2x")
  system(paste0('curl ', url), intern = T)
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
         tableOutput("tickerInfo"),
         textOutput("description")
      ),
      # Show rBokeh plot of time-series and Forecast and Data
      mainPanel(
        tabsetPanel(
          tabPanel("Historical", rbokehOutput("tsPlot")),
          tabPanel("Short-term Forecast", rbokehOutput("forecast")),
          tabPanel("Data", DT::dataTableOutput("dataTable"))
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
    "DESCRIPTION of WIKI data\nhttps://www.quandl.com/databases/WIKIP\nEnd of day stock prices, dividends and splits for 3,000 US companies, curated by the Quandl community and released into the public domain."
  )
  # grab the raw data from Quandl
  data <- reactive({
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
    data() %>%
      figure(xlab = "", ylab = "Price at Closing (Daily)") %>%
      ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close))
  })
  output$forecast <- renderRbokeh({
    data() %>%
      filter(Date > (max(Date, na.rm = T) - months(3))) %>%
      figure(xlab = "", ylab = "Price at Closing (Daily") %>%
      ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close))
  })
  output$dataTable <- DT::renderDataTable({
    tmp <- data()
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

