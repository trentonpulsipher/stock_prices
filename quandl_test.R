
# install.packages("Quandl")
library("Quandl")
library(tidyverse)
library(lubridate)
library(rbokeh)

# my API key
Quandl.api_key("1xrGSwDRn_MapbqUyt2x")

# Quandl("NSE/OIL", 
#        # ticker = "AAPL", 
#        collapse = "daily", 
#        column_index = 5, 
#        start_date="2018-10-01", 
#        api_key = "1xrGSwDRn_MapbqUyt2x") %>% 
#   as.tibble() %>% head(15)
# Quandl.datatable("ZACKS/FC", ticker = "AAPL", per_type = "D") %>% as.tibble() %>% head(20)

# would need to define a wrapper function to create the url below given the ticker

tmp <- system('curl "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?collapse=daily&column_index=4&api_key=1xrGSwDRn_MapbqUyt2x"',
       intern = T)
tmp %>%
  as.tibble() %>%
  slice(-1) %>% # remove the header line
  mutate(Date = ymd(unlist(lapply(str_split(value, ","), "[[", 1))),
         Close = as.numeric(unlist(lapply(str_split(value, ","), "[[", 2)))) %>%
  figure(xlab = "", ylab = "Price at Closing (Daily)") %>%
    ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close))


# c("Date", "Open", "High", "Low", "Close", "Volume", "Ex-Dividend", "Split Ratio", 
  # "Adj. Open", "Adj. High", "Adj. Low", "Adj. Close", "Adj. Volume")
