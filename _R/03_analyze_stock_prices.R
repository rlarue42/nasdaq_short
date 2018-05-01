library(tidyquant)
library(tidyverse)
library(ggplot2)
library(quantmod)
library(rebus)


# read raw data -----------------------------------------------------------

df_stock_data <-  read_csv(file = "_raw_data/df_stock_data_2018-04-19.csv")



# from long to  wide format 
to_wide <- function(data, rt) {
        data %>%
                filter(sub.section == rt) %>% 
                drop_na() %>% 
                spread(category, value)

}


# plot stock prices -----------------------------------------------------------

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


# find peer group outperforming stocks  -----------------------------------

df_sma <- df_stock_data %>%
        filter(ratio == "adjusted") %>% 
        split(.$symbol) %>% 
        map(drop_na) %>% 
        map(safely(mutate),sma30 = SMA(value,n = 30),
                           sma90 = SMA(value,n = 90),
                           sma180 = SMA(value,n = 180),
                           sma360 = SMA(value,n = 360),
                           sma1000 = SMA(value,n = 1000),
            trend_30 = case_when(sma30 > value ~ "below",
                                 sma30 < value ~ "above"),
            trend_90 = case_when(sma90 > value ~ "below",
                                 sma90 < value ~ "above"),
            trend_180 = case_when(sma180 > value ~ "below",
                                  sma180 < value ~ "above"),
            trend_360 = case_when(sma360 > value ~ "below",
                                  sma360 < value ~ "above"),
            trend_1000 = case_when(sma1000 > value ~ "below",
                                  sma1000 < value ~ "above")) %>% 
        map_df("result")


df_elevated <- df_sma %>%
        split(.$symbol) %>%
        map(tail, n = 1) %>% 
        map_df(filter, trend_1000 == "above", trend_30 == "above")  %>% 
        select(symbol) %>%
        pull()


chrts <- df_sma %>%
                filter(symbol %in% df_elevated) %>%  
                select_at(c(1:2,4:9)) %>% 
                gather(ratio,value,3:length(.)) %>% 
                split(.$symbol) %>% 
                map( ~
                ggplot(data = . , 
                       aes(date, value, col = ratio)) +
                geom_line() +
                facet_wrap(~symbol, scales = "free") +
                theme_minimal() +
                theme(legend.position = "bottom") + 
                scale_color_tq() + 
                        labs(title = paste0(.$symbol),
                             subtitle = "above 360 & 30 day SMA"))  
                

chrts[[19]]

tq_mutate_fun_options()


# stoch
