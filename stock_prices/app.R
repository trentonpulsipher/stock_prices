
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



# Define UI for application that draws a histogram
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
      # Show rBokeh plot of time-series
      mainPanel(
         rbokehOutput("tsPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tickerInfo <- renderTable(allTickers %>% 
                                    filter(Ticker == input$ticker) %>%
                                    select(Name, Sector, Industry))
  output$description <- renderText(
    "DESCRIPTION of WIKI data\nhttps://www.quandl.com/databases/WIKIP\nEnd of day stock prices, dividends and splits for 3,000 US companies, curated by the Quandl community and released into the public domain."
  )

   output$tsPlot <- renderRbokeh({   
     # grab the raw data from Quandl
     get_raw_data(input$ticker) %>%
       as.tibble() %>%
       slice(-1) %>% # remove the header line
       mutate(Date = ymd(unlist(lapply(str_split(value, ","), "[[", 1))),
              Close = as.numeric(unlist(lapply(str_split(value, ","), "[[", 5)))) %>%
       figure(xlab = "", ylab = "Price at Closing (Daily)") %>%
       ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

# library(rsconnect)
# rsconnect::deployApp('~Documents/Development/R/stock_prices/stock_prices')

## NOTES:
# https://www.quandl.com/databases/WIKIP
# DESCRIPTION of WIKI data: End of day stock prices, dividends and splits for 3,000 US companies, curated by the Quandl community and released into the public domain.
# could vary more than just closing price
# could provide shorter window (say last year) if desired
