###################################################################################
# created: 03/12/2022
# by: Saani Rawat
# purpose: automate point-and-click to download Call Report data from FFIEC website
# Log:
# 1. 03/12/2022: used to download call report txt files from 2001 to 2021
###################################################################################

# load packages
library(tidyverse)
library(RSelenium)
library(netstat)
library(beepr)

#start the server
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = "99.0.4844.51",
                             verbose = FALSE,
                             port = free_port())

class(rs_driver_object)

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()

# maximize window size
remDr$maxWindowSize()

# url
website <- "https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx"
    
# navigate to website
remDr$navigate(website)

# finding and clicking on "Single Period" 
CR_single <- remDr$findElement(using = 'xpath', "//select[@id='ListBox1']/option[@value= 'ReportingSeriesSinglePeriod']")
CR_single$clickElement()

# create a list for all the "coded" dates in html
# dt_list <- c("'127'", "'129'", "'125'","'0'","'1'")
dt_list <- paste0("'",as.character(seq(from = 1, to = 130, by = 1)),"'")

for (date in dt_list){
    # wait..
    Sys.sleep(2)
    try({
        # find date and select a particular value
        CR_date <- remDr$findElement(using = 'xpath', paste0("//select[@id='DatesDropDownList']/option[@value = ",date,"]") )
        CR_date$clickElement()
    Sys.sleep(2)
    
    # download this text file
    download <- remDr$findElement(using = 'id', "Download_0")
    download$clickElement()    
    }
    , silent = TRUE
    )
}

# finished
beep("mario")

