library(DT)
library(shiny)
library(highcharter)
library(magrittr)
library(httr)
library(jsonlite)
library("data.table")
library(googlesheets)
library(tidyr)
library(dplyr,warn.conflicts = FALSE, quietly = TRUE)
library(formattable)
library(knitr)
library(shinydashboard)
library(lubridate)
library(readr)
library(ggplot2)
library(lazyeval)
library(tidyverse)
library(plotly)
library(V8)
library(shinyjs)
library(tibble)
library(tidyverse)
#options(error = 999)
appCSS <- "
#loading {
  position: fixed;
left: 50%;
top: 50%;
z-index: 1;
width: 150px;
height: 150px;
margin: -75px 0 0 -75px;
border: 16px solid #f3f3f3;
border-radius: 50%;
border-top: 16px solid #3498db;
width: 120px;
height: 120px;
-webkit-animation: spin 2s linear infinite;
animation: spin 2s linear infinite;
}
@-webkit-keyframes spin {
0% { -webkit-transform: rotate(0deg); }
100% { -webkit-transform: rotate(360deg); }
}
@keyframes spin {
0% { transform: rotate(0deg); }
100% { transform: rotate(360deg); }
}
"

# Define JS and CSS files ----
jsfile <- "getdata.js"
cssfile <- "style.css"
js_scroll_file <-'scroll.js'
allObjs <- c("mydata","state","resolutions","refresh","csvurl",
             "DashboardExpandedName","colours","theme","expandBookMarkOpts",
             "bookmarkType","objBookmarks","includeExcluedBk","type","sortData",
             "map_resolution")

# UIside custom scripts ----
colours <- c("blue","red","black","orange","yellow","purple","brown", "pink")
themes <- c(FALSE, "fivethirtyeight", "economist","darkunica","gridlight","sandsignika","null","handdrwran","chalk")


url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ5Zgn_CarwUu09Y_0r9Zc9TB1ZAo5Qh1UjhdnhS_h6RfNq7QolP-bA-56dX31mWG-vtK4OEENcrnQ-/pub?gid=0&single=true&output=csv"
