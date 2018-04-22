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

df_financials <- df_key_ratios %>%
                 select(-group) %>%
                filter(section == "Financials") %>%
                split(.$sub.section) %>% 
                map_df(spread, category, value)

df_profitability <- df_key_ratios %>%
                select(-group) %>%
                filter(section == "Profitability") %>%
                split(.$sub.section) %>% 
                map(spread, category, value)

df_profitability %>% map(Profitability)


stats <- df_financials %>% select_if(is.numeric) %>% skimr::skim()



# goals! ------------------------------------------------------------------

*Most leveraged (short term)
*Declining sales
*stock prices above trend




# actionable steps --------------------------------------------------------




