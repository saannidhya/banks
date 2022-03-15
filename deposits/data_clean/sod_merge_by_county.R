# Purpose: to merge summary of deposits (SOD) data by county (using FIPS code)
# Author: Saani Rawat, University of Cincinnati
# Last changed: 17th Sep, 2021

library(tidyverse)
library(purrr)
library(here)
library(janitor)
library(readr)

# reading in and storing datasets into an R object
dataset_names <- stringr::str_remove(list.files(here::here("data","fdic","SOD"),
                                                pattern = ".csv",
                                                recursive = TRUE),
                                     paste0(".", "csv"))

datasets <- purrr::map(list.files(here::here("data","fdic","SOD"),
                                  pattern = ".csv",
                                  recursive = TRUE,
                                  full.names = TRUE),
                       readr::read_csv)

names(datasets) <- dataset_names


# deposits by country. Note: only considering full service branches i.e. code 11, 12, 13 (see sod_variables_definitions.xlsx by FDIC)
summarise_by_county <- function(data){
    
    clean_df <- data.frame(data) %>%
        janitor::clean_names()
    
    clean_df$depsumbr <- as.numeric(gsub(",", "", clean_df$depsumbr))
    
    df_summarize <- clean_df %>%
        select(year, stcntybr, cntynamb, stnamebr, brsertyp, depsumbr) %>%
        filter(brsertyp %in% c(11, 12, 13)) %>%
        mutate(stnamebr = tolower(stnamebr), cntynamb = tolower(cntynamb), fips = str_pad(as.character(stcntybr), 5, pad = "0")) %>%        
        group_by(year, fips, cntynamb, stnamebr) %>%
        summarize(deposits = sum(depsumbr))    
    
    return(df_summarize)
}

# use function for all years
sod_merge_by_county <- c()
for (df in dataset_names){
    # print(glue::glue("started: {df}"))
    df_sum <- summarise_by_county(datasets[[df]])
    sod_merge_by_county <- bind_rows(sod_merge_by_county, df_sum)
}
sod_merge_by_county <- sod_merge_by_county %>%
    arrange(cntynamb, year)

save(sod_merge_by_county, 
     file = glue::glue("{here::here('data','data_clean')}/sod_merge_by_county.RData"))


# load(glue::glue("{here::here('data','data_clean')}/sod_merge_by_county.RData"))


