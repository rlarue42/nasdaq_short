library(tidyquant)
library(tidyverse)
library(ggplot2)
library(quantmod)
library(rebus)


# read raw data -----------------------------------------------------------

df_stock_data <-  read_csv(file = "_raw_data/df_stock_data_2018-04-19.csv")



# create lists ------------------------------------------------------------

df_stock_data %>% colnames()
df_stock_data$ratio %>% unique()


# from long to  wide format 
to_wide <- function(data, rt) {
        data %>%
                filter(sub.section == rt) %>% 
                drop_na() %>% 
                spread(category, value)

}


# analyze  stock prices -----------------------------------------------------------

# create chart databand
chart_ts_bb <- function(data, stock.ticker) {
        
        lst_tic  <- stock.ticker 
        
        df_stock_data %>%
                filter(symbol %in% stock.ticker) %>%
                spread(ratio, value) %>%  
                ggplot(aes(x = date, y = close,group = symbol, col = symbol)) +
                geom_line() +           
                geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 120) +
                labs(title = "Stock price timeseries") + 
                theme_minimal()
}



# Plot stock price & bbands
chart_ts_bb(data = df_stock_data, stock.ticker = c("BBSI"))
chart_ts_bb(data = df_stock_data, stock.ticker = c("AAPL","VNET"))

