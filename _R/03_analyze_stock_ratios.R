library(tidyquant)
library(tidyverse)
library(ggplot2)
library(quantmod)
library(rebus)


# read raw data -----------------------------------------------------------

df_key_ratios <-  read_csv(file = "_raw_data/df_key_ratios_2018-04-19.csv")

# create lists ------------------------------------------------------------

lst_sections       <- df_key_ratios %>% pull(section) %>% unique()
lst_sub_sections   <- df_key_ratios %>% pull(sub.section) %>% unique()
lst_stocks         <- df_key_ratios %>% pull(symbol) %>% unique()


# from long to  wide format 
to_wide <- function(data, rt) {
        data %>%
                filter(sub.section == rt) %>% 
                drop_na() %>% 
                spread(category, value)
        
}

df_key_ratios_wide <- map(to_wide, .x = lst_key_ratios, data = df_key_ratios)

# analyze  stock key ratio's ------------------------------------------------------


# extract sections
df_val <-  df_key_ratios %>%
        select(-group) %>%
        filter(section == "Valuation Ratios") 

df_financials <- df_key_ratios %>%
                 select(-group) %>%
                filter(section == "Financials") %>%
                split(.$category) %>% 
                map_df(spread, category, value)

df_profitability <- df_key_ratios %>%
                select(-group) %>%
                filter(section == "Profitability") %>%
                split(.$sub.section) %>% 
                map(spread, category, value)

# valuation based filter algorithm (VBFA)

calc_stock_valuation_stats <- function(data) {
        data <- data %>% 
                group_by(category) %>%
                summarise(last = last(value),
                          year_ago = nth(value,-1),
                          mean = mean(value,na.rm = T)) %>% 
                ungroup()
        
        
}

calc_index_valuation_stats <- function(data) {
        data <- data %>% 
                group_by(date) %>%
                summarise(index_median = median(value,na.rm = T)) %>%
                drop_na()
        return(data)
}




df_val_alg_stock <- df_val %>%
                        nest(-symbol) %>%
                        mutate(summary = map(data, calc_stock_valuation_stats))


df_val_alg_index <- df_val %>%
                        nest(-category) %>%
                        mutate(summary = map(data, calc_index_valuation_stats))

                
df_val_alg_final <- df_val_alg_stock %>%
        unnest(summary) %>%
        left_join(df_val_alg_index %>%
                         unnest(summary) %>%
                         filter(date == "2017-12-29")) %>% 
        select(symbol, category, last, index_median) %>% 
        group_by(category) %>% 
        mutate(to_index = last / index_median,
               to_index_rank = ntile(to_index,10)) %>% 
        ungroup() %>% 
        group_by(symbol) %>% 
        mutate(val_alg_score = mean(to_index_rank,na.rm = T)) %>% 
        arrange(-val_alg_score)
        

# goals! ------------------------------------------------------------------

*Most leveraged (short term)
*Declining sales
*stock prices above trend


# actionable steps --------------------------------------------------------




