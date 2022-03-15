# Purpose: to merge summary of deposits (SOD) data by State, by year
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


# deposits by state. Note: only considering full service branches i.e. code 11, 12, 13 (see sod_variables_definitions.xlsx by FDIC)
summarise_by_state <- function(data){
    
    clean_df <- data.frame(data) %>%
        janitor::clean_names()
    
    clean_df$depsumbr <- as.numeric(gsub(",", "", clean_df$depsumbr))
    
    df_summarize <- clean_df %>%
        select(year, stnamebr, brsertyp, depsumbr) %>%
        filter(brsertyp %in% c(11, 12, 13)) %>%
        mutate(stnamebr = tolower(stnamebr)) %>%
        group_by(year, stnamebr) %>%
        summarize(deposits = sum(depsumbr), num_branches = n())    
    
    return(df_summarize)
}

# length(unique(datasets$ALL_2021$STNAMEBR)) #59
# length(unique(datasets$ALL_2021$CNTYNAMB)) # 1918. Some counties do not have full 28 yrs of data

# use function for all years
sod_merge_by_state <- c()
for (df in dataset_names){
    df_sum <- summarise_by_state(datasets[[df]])
    sod_merge_by_state <- bind_rows(sod_merge_by_state, df_sum)
}
sod_merge_by_state <- sod_merge_by_state %>%
                            arrange(stnamebr, year)

save(sod_merge_by_state, 
     file = glue::glue("{here::here('data','data_clean')}/sod_merge_by_state.RData"))


# load(glue::glue("{here::here('data','data_clean')}/sod_merge_by_state.RData"))




