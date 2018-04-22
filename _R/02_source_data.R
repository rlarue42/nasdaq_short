library(tidyquant)
library(tidyverse)
library(ggplot2)
library(quantmod)
library(rebus)

#devtools::install_github("business-science/tidyquant")


# define stock universum --------------------------------------------------

lst_get_option <- tq_get_options()
lst_nasdaq <- tq_exchange("NASDAQ")

# reduce investable universe to +500 market cap & sector Technology 
chrkt <- "\\$" 

lst_nasdaq_filtered <- lst_nasdaq %>% 
        filter(sector == "Technology") %>% 
        mutate(market.cap = str_replace_all(market.cap, "M","000000"),
               market.cap = str_replace_all(market.cap, "B","000000000"),
               market.cap = str_replace_all(market.cap, chrkt,""),
               market.cap = as.integer(market.cap)) %>% 
        filter(market.cap > 500) # now just 47 stocks!

list_stock <- lst_nasdaq_filtered %>% pull(symbol) %>% unique()

# retrive nasdaq tradet stock  --------------------------------------------

df_database   <- map(tq_get,.x = list_stock, get = c("key.ratios","stock.prices"))
df_key_ratios <- df_database %>% map(unnest,key.ratios) %>% map_df(unnest,data) 
df_stock_data <- df_database %>% map_df(unnest,stock.prices) %>% gather(ratio, value,3:length(.))


# store the database ------------------------------------------------------

write_csv(df_key_ratios, path = paste0("_raw_data/df_key_ratios_",Sys.Date(),".csv"))
write_csv(df_stock_data, path = paste0("_raw_data/df_stock_data_",Sys.Date(),".csv"))



