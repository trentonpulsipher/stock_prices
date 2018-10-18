
# install.packages("Quandl")
# devtools::install_github("business-science/sweep")
library(Quandl)
library(tidyverse)
library(lubridate)
library(rbokeh)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)


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

get_raw_data <- function(ticker) {
  # reminder: my Quandl API_key is hard coded below
  url <- paste0("https://www.quandl.com/api/v3/datasets/WIKI/", ticker, "/data.csv?collapse=daily&column_index=4&api_key=1xrGSwDRn_MapbqUyt2x")
  system(paste0('curl ', url), intern = T)
}
tmp <- get_raw_data("AAPL")

  
# tmp <- system('curl "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?collapse=daily&column_index=&api_key=1xrGSwDRn_MapbqUyt2x"',
#        intern = T)
plot_timeseries <- tmp %>%
  as.tibble() %>%
  slice(-1) %>% # remove the header line
  mutate(Date = ymd(unlist(lapply(str_split(value, ","), "[[", 1))),
         Close = as.numeric(unlist(lapply(str_split(value, ","), "[[", 5)))) %>%
  figure(xlab = "", ylab = "Price at Closing (Daily)") %>%
    ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close))


# c("Date", "Open", "High", "Low", "Close", "Volume", "Ex-Dividend", "Split Ratio", 
  # "Adj. Open", "Adj. High", "Adj. Low", "Adj. Close", "Adj. Volume")


# grab all tickers
allTickers <- system('curl "https://www.quandl.com/api/v3/databases/WIKI/codes?api_key=1xrGSwDRn_MapbqUyt2x"', inter = T)
allTickers <- read_csv("~/Downloads/secwiki_tickers.csv") %>%
  filter(str_detect(Price, "WIKI"))


data2use <- get_raw_data("AAPL") %>%
  as.tibble() %>%
  slice(-1) %>% # remove the header line
  mutate(
    Date = ymd(unlist(lapply(str_split(value, ","), "[[", 1))),
    Close = as.numeric(unlist(lapply(str_split(value, ","), "[[", 5))),
    Volume = as.numeric(unlist(lapply(str_split(value, ","), "[[", 6)))
  ) %>%
  select(Date, Close, Volume) %>%
  arrange(desc(Date))

forecasted <- data2use %>% 
  # use the last 3 years for prediction
  filter(Date > (max(Date, na.rm = T) - years(3))) %>%
  # convert from tibble to ts structure
  tk_ts(select = Close) %>%
  # Exponential smoothing (error, trend, seasonal) model
  ets() %>%
  forecast(h = 30) %>%
  # back to tibble
  tk_tbl(timetk_idx = TRUE) %>%
  # convert back to date
  mutate(Date = max(data2use$Date) + days(1:30)) %>%
  rename(Forecast = `Point Forecast`,
         CI95LB = `Lo 95`,
         CI95UB = `Hi 95`) %>%
  select(Date, Forecast, CI95LB, CI95UB)

  
data %>%
  filter(Date > (max(Date, na.rm = T) - years(3))) %>%
  ggplot(aes(x = Date, y = Close)) +
    geom_point(color = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = Date, y = Forecast, ymin = CI95LB, ymax = CI95UB), 
                data = forecasted, fill = "red", alpha = 0.5) +
    geom_line(aes(x = Date, y = Forecast), data = forecasted, color = "red") +
    theme_bw() +
    labs(x = "", 
         y = "Price at Closing (Daily)", 
         title = "30-day Forecast (red) with 95% Confidence Interval Bands")
  
  
# figure(xlab = "", ylab = "Price at Closing (Daily)") %>%
#   ly_points(Date, Close, hover = list("Date" = Date, "Closing Price" = Close)) %>%
#   ly_lines(Date, Forecast, data = forecasted)


AR1 <- arima(diff(data2use %>% 
                   filter(Date > (max(Date, na.rm = T) - years(1))) %>% 
                   pull(Close)), order = c(1, 0, 0))
RW <- arima(diff(data2use %>%
                   filter(Date > (max(Date, na.rm = T) - years(1))) %>% 
                   pull(Close)), order = c(0, 1, 0))
# random walk model


function(lastPrice = data2use$Close[length(data2use$Close)],
         days = 30, #input$numDaysForecast
         numMCs = 500) {
  # calculate the mean for the random walk
#  mu <- data2use %>% filter(Date > "2018-01-01") %>% pull(Close) %>% mean(na.rm = T)
  # calculate the stdev for the random walk
  sig <- data2use %>% filter(Date > "2018-01-01") %>% pull(Close) %>% sd(na.rm = T)

  out <- list()
  for(i in 1:numMCs) {
    out[[i]] <- rep(NA, length = days + 1)
    out[[i]][1] <- lastPrice
    for(j in 2:(days+1)) {
      out[[i]][j] = out[[i]][j-1] + rnorm(1, 0, sig) + drift
    }
  }
  apply(do.call(rbind,out)[,-1], 2, quantile, probs = 0.5)
}



