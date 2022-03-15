

# install packages
library(tidyverse)
library(haven)
library(purrr)
library(Rbearcat)
library(readr)

# specifying all paths
call_path <- file.path(here::here(), "shared","data","ffiec","call_reports")
f <- list.files(path = file.path(here::here(), "shared","data","ffiec","call_reports"),
                pattern = "txt",
                recursive = TRUE,
                full.names = TRUE)

###################################################################################
## Importing csv look up tables
###################################################################################

l <- list.files(path = file.path(here::here(), "shared","data","ffiec","call_reports"),
                pattern = "csv",
                recursive = TRUE,
                full.names = TRUE)

l_names <- gsub(".*/","",stringr::str_remove(l,".csv"))

lookups <- purrr::map(l, readr::read_csv)
lookups <- setNames(lookups, l_names)

bs_vars <- c("IDRSSD",lookups$bal_sheet_lookup[["mdrm_code"]])
is_vars <- c("IDRSSD",lookups$inc_statement_lookup[["mdrm_code"]])
cap_vars <- c("IDRSSD",lookups$reg_capital_lookup[["mdrm_code"]])


###################################################################################
# Importing Balance Sheet data
###################################################################################

# finding balance sheet names
f_ <- f[grepl(x = f, pattern = " RC ")]

# loading in balance sheet datasets
rc_df <- purrr::map(f_, read.delim)

rc_df_names <- stringr::str_remove(f_,".txt")
rc_df_names <- gsub(".*/","",rc_df_names)
rc_df_names <- gsub(" ","_",rc_df_names)
rc_df_names <- gsub(".*_Schedule_","",rc_df_names)

rc_df <- setNames(rc_df, rc_df_names)

# storing dates as character vector
rc_dates <- gsub("RC_","", names(rc_df))

# removing descriptions (1st observation), assigning tibble and adding date column
for (i in  1:length(rc_df)) {
    rc_df[[i]] <- as_tibble(rc_df[[i]][2:nrow(rc_df[[i]]),]) %>%
        select_if(names(.) %in% bs_vars) 
    rc_df[[i]]$date <- rc_dates[i]
}

# variable lookup table
# mdrm_desc <- as_tibble(rc_df$RI_03312001[1,]) %>%
#     pivot_longer(cols = c(-1), names_to = "mdrm_code") %>%
#     pivot_wider(names_from = 1) %>%
#     rename("description" = "NA" ) 

# ccar was introduced in 2011
saveRDS(rc_df, file = paste0(call_path,"/bal_sheet.RData"))

# rc_df <- readRDS(paste0(call_path,"/bal_sheet.RData"))


###################################################################################
# Importing Income Statement data
###################################################################################

# finding Income Statement names
fi_ <- f[grepl(x = f, pattern = " RI ")]

# loading in Income statement datasets
ri_df <- purrr::map(fi_, read.delim)

ri_df_names <- stringr::str_remove(fi_,".txt")
ri_df_names <- gsub(".*/","",ri_df_names)
ri_df_names <- gsub(" ","_",ri_df_names)
ri_df_names <- gsub(".*_Schedule_","",ri_df_names)

ri_df <- setNames(ri_df, ri_df_names)

# storing dates as character vector
ri_dates <- gsub("RI_","", names(ri_df))

# removing descriptions (1st observation), assigning tibble and adding date column
for (i in  1:length(ri_df)) {
    ri_df[[i]] <- as_tibble(ri_df[[i]][2:nrow(ri_df[[i]]),]) %>%
        select_if(names(.) %in% is_vars) 
    ri_df[[i]]$date <- ri_dates[i]
}

saveRDS(ri_df, file = paste0(call_path,"/inc_statement.RData"))

# ri_df <- readRDS(paste0(call_path,"/inc_statement.RData"))

# d <- df_rc[2:nrow(df_rc),] %>%
#         select(IDRSSD, RCFD2170, RCON2170) %>%
#         mutate(across(2:3, as.numeric)) %>%
#         mutate(tot_assets = if_else(is.na(RCFD2170), as.numeric(RCON2170), as.numeric(pmax(RCFD2170, RCON2170)))) %>%
#         mutate(l_tot_assets = log(if_else(is.na(RCFD2170), as.numeric(RCON2170), as.numeric(pmax(RCFD2170, RCON2170)))))
# 
# ggplot(data = d, aes(x = tot_assets)) +
#     geom_histogram() + 
#     scale_x_continuous(labels = scales::comma)
# 
# ggplot(data = d, aes(x = l_tot_assets)) +
#     geom_histogram(bins = 30) + 
#     scale_x_continuous(labels = scales::comma)


# dt_list <- format(seq(as.Date("2003-12-31"), as.Date("2021-12-31"), by = "year"), format = "%m%d%Y")
# for (date in dt_list){
#     file.remove(paste0(call_path,"/FFIEC CDR Call Bulk All Schedules ", date,"/Readme.txt"))
# }


###################################################################################
# Importing Regulatory Capital data
###################################################################################

c_ <- f[grepl(x = f, pattern = " RCR | RCRI ")]

# loading in Regulatory capital datasets
cap_df <- purrr::map(c_, read.delim)

cap_df_names <- stringr::str_remove(c_,".txt")
cap_df_names <- gsub(".*/","",cap_df_names)
cap_df_names <- gsub(" ","_",cap_df_names)
cap_df_names <- gsub(".*_Schedule_","",cap_df_names)

cap_df <- setNames(cap_df, cap_df_names)

# storing dates as character vector
cap_dates <- gsub("RCR_|\\(1_of_2\\)|\\(2_of_2\\)","", names(cap_df))


# removing description (1st observation), assigning tibble and adding date column
for (i in  1:length(cap_df)) {
    cap_df[[i]] <- as_tibble(cap_df[[i]][2:nrow(cap_df[[i]]),]) %>%
        select_if(names(.) %in% cap_vars) 
    cap_df[[i]]$date <- cap_dates[i]
}


saveRDS(cap_df, file = paste0(call_path,"/capital.RData"))

# ri_df <- readRDS(paste0(call_path,"/inc_statement.RData"))




###################################################################################
# Creating Balance Sheet data file
###################################################################################

rc_df$