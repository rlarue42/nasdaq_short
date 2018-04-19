library(tidyquant)
library(tidyverse)
library(ggplot2)
library(quantmod)
library(rebus)


# read raw data -----------------------------------------------------------

df_key_ratios <-  read_csv(file = "_raw_data/df_key_ratios_2018-04-19.csv")
df_stock_data <-  read_csv(file = "_raw_data/df_stock_data_2018-04-19.csv")



# create lists ------------------------------------------------------------

lst_key_ratios <- df_key_ratios %>% pull(sub.section) %>% unique()
lst_stocks     <- df_key_ratios %>% pull(symbol) %>% unique()



# from long to  wide format 
to_wide <- function(data, rt) {
        data %>%
                filter(sub.section == rt) %>% 
                drop_na() %>% 
                spread(category, value)

}

df_key_ratios_wide <- map(to_wide, .x = lst_key_ratios, data = df_key_ratios)
        
# analyze  ----------------------------------------------------------------

df_stock_data %>%
        filter(ratio == "adjusted") %>% 
        ggplot(aes(date, value, group = symbol)) + geom_line()



df_key_ratios_wide[[1]] %>%
        drop_na() %>% 
        group_by(symbol) %>% 
        transmute()
        ggplot2::ggplot(aes(date, value))  +
        geom_line() + 
        facet_wrap(~category, scales = "free")
