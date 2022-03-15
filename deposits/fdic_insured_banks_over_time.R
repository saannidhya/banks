# Plotting FDIC-insured banks history

library(here)
library(tidyverse)
library(Rbearcats)

ins_banks <- readr::read_csv(glue::glue(here::here(),"/bank-data.csv"))

# plotting n.o of FDIC-insured banks over time (highlighting recession periods based on GDP)
# source fot bank data: https://banks.data.fdic.gov/explore/historical/?displayFields=STNAME%2CTOTAL%2CBRANCHES%2CNew_Char&selectedEndDate=2020&selectedReport=CBS&selectedStartDate=1934&selectedStates=0&sortField=YEAR&sortOrder=desc
# source for NBER recession dates: https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions
Rctmd::util_plt_line(df = ins_banks, 
                     x = YEAR, 
                     y = TOTAL, 
                     x_highlight_min = c(1981, 1990, 2001, 2007, 2020), 
                     x_highlight_max = c(1982, 1991, 2002, 2009, 2021), 
                     y_lab = "N.o of FDIC insured banks")
# Starting 1986, n.o of banks in U.S started declining really fast


# plotting n.o of FDIC-insured banks over time (highlighting Savings and Loans Crisis)
Rctmd::util_plt_line(df = ins_banks, 
                     x = YEAR, 
                     y = TOTAL, 
                     x_highlight_min = c(1985, 2007), 
                     x_highlight_max = c(1995, 2011), 
                     y_lab = "N.o of FDIC insured banks")
# Starting 1986, n.o of banks in U.S started declining really fast



